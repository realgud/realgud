;;; Copyright (C) 2010-2013 Rocky Bernstein <rocky@gnu.org>
;;  tracks shell output

(eval-when-compile (require 'cl))
(require 'shell)

(require 'load-relative)
(require-relative-list
 '("core"   "helper" "track" "loc" "lochist" "file"
   "fringe" "window" "regexp" "menu" "backtrace-mode"
   "send"   "shortkey") "realgud-")

(require-relative-list  '("buffer/command") "realgud-buffer-")

(defvar realgud-track-mode-map
  (let ((map  (realgud-populate-debugger-menu (make-sparse-keymap))))
    (define-key map [M-right]	'realgud-track-hist-newest)
    (define-key map [M-down]	'realgud-track-hist-newer)
    (define-key map [M-up]	'realgud-track-hist-older)
    (define-key map [M-print]	'realgud-track-hist-older)
    (define-key map [M-S-down]	'realgud-track-hist-newest)
    (define-key map [M-S-up]	'realgud-track-hist-oldest)
    (define-key map "\C-cS" 'realgud-window-src-undisturb-cmd)
    map)
  "Keymap used in `realgud-track-minor-mode'.")

(set-keymap-parent realgud-track-mode-map shell-mode-map)


;; FIXME figure out if I can put this in something like a header file.
(declare-function realgud-track-set-debugger (debugger-name &optional hash))

(define-minor-mode realgud-track-mode
  "Minor mode for tracking debugging inside a process shell."
  :init-value nil
  :global nil
  :group 'realgud

  :lighter
  (:eval (progn
	   (concat " "
		   (if (realgud-cmdbuf-info-set?)
		       (realgud-sget 'cmdbuf-info 'debugger-name)
		     "dbgr??"))))

  :keymap realgud-track-mode-map
  ;; Setup/teardown
  (realgud-track-mode-setup realgud-track-mode)
  )

;; FIXME: this should have been picked up by require'ing track.
(defvar realgud-track-divert-string)

(defun realgud-track-mode-setup (mode-on?)
  "Called when entering or leaving `realgud-track-mode'. Variable
MODE-ON is a boolean which specifies if we are going into or out
of this mode."
  (if mode-on?
      (let ((process (get-buffer-process (current-buffer))))
	(unless process
	  (setq realgud-track-mode nil)
	  (error "Can't find a process for buffer %s" (current-buffer)))

	(setq realgud-track-divert-string "")
	(setq realgud-track-mode 't)

	;; FIXME: save and chain process-sentinel via
	;; (process-sentinel (get-buffer-process (current-buffer)))
	(set-process-sentinel process 'realgud-term-sentinel)
	(unless (and (realgud-cmdbuf-info-set?)
		     (realgud-sget 'cmdbuf-info 'debugger-name))
	  (call-interactively 'realgud-track-set-debugger))
	(if (boundp 'comint-last-output-start)
	    (progn
	      (realgud-cmdbuf-info-prior-prompt-regexp= comint-prompt-regexp)
	      (realgud-cmdbuf-info-divert-output?= nil)
	      (let* ((regexp-hash
		      (and (realgud-cmdbuf-info? realgud-cmdbuf-info)
			   (realgud-sget 'cmdbuf-info 'regexp-hash)))
		     (prompt-pat (and regexp-hash
				      (gethash "prompt" regexp-hash))))
		(if prompt-pat
		    (setq comint-prompt-regexp
			    (realgud-loc-pat-regexp prompt-pat)))))
	  (set-marker comint-last-output-start (point)))

	(add-hook 'comint-output-filter-functions
		  'realgud-track-comint-output-filter-hook)
	(add-hook 'eshell-output-filter-functions
		  'realgud-track-eshell-output-filter-hook)
	(run-mode-hooks 'realgud-track-mode-hook))
    (progn
      (if (and (boundp 'comint-last-output-start) realgud-cmdbuf-info)
	(setq comint-prompt-regexp
	   (realgud-sget 'cmdbuf-info 'prior-prompt-regexp))
	)
      (realgud-fringe-erase-history-arrows)
      (remove-hook 'comint-output-filter-functions
      		   'realgud-track-comint-output-filter-hook)
      (remove-hook 'eshell-output-filter-functions
		    'realgud-track-eshell-output-filter-hook)
      (let* ((cmd-process (get-buffer-process (current-buffer)))
	     (status (if cmd-process
			 (list (propertize (format ":%s"
						   (process-status cmd-process))
			    'face 'debugger-running))
		       ""))
	     )
	(setq mode-line-process status)
	;; Force mode line redisplay soon.
	(force-mode-line-update)
	;; FIXME: This is a workaround. Without this, we comint doesn't
	;; process commands
	(comint-mode)
	)

      ;; FIXME: restore/unchain old process sentinels.
      )
    )
  )

;; For name == "trepan", produces:
;;   (defvar trepan-track-mode nil
;;     "Non-nil if using trepan track-mode ... "
;;   (defvar trepan-track-mode-map (make-sparse-keymap))
;;   (set-keymap-parent trepan-track-mode-map realgud-track-mode-map)
;;   (defvar trepan-short-key-mode-map (make-sparse-keymap))
;;   (set-keymap-parent trepan-short-key-mode-map realgud-short-key-mode-map)
(defmacro realgud-track-mode-vars (name)
  `(progn
     (defvar ,(intern (concat name "-track-mode")) nil
	,(format "Non-nil if using %s-track-mode as a minor mode of some other mode.
Use the command `%s-track-mode' to toggle or set this variable." name name))
     (defvar ,(intern (concat name "-track-mode-map")) (make-sparse-keymap)
       ,(format "Keymap used in `%s-track-mode'." name))
     (set-keymap-parent ,(intern (concat name "-track-mode-map")) realgud-track-mode-map)
     (defvar ,(intern (concat name "-short-key-mode-map")) (make-sparse-keymap))
     (set-keymap-parent ,(intern (concat name "-short-key-mode-map")) realgud-short-key-mode-map)
    ))

;; FIXME: The below could be a macro? I have a hard time getting
;; macros right.
(defun realgud-track-mode-body(name)
  "Used in by custom debuggers: pydbgr, trepan, gdb, etc. NAME is
the name of the debugger which is used to preface variables."
  (realgud-track-set-debugger name)
  (funcall (intern (concat "realgud-define-" name "-commands")))
  (if (intern (concat name "-track-mode"))
      (progn
	(setq realgud-track-mode 't)
	(run-mode-hooks (intern (concat name "-track-mode-hook"))))
    (progn
      (setq realgud-track-mode nil)
      )))

(defun realgud-track-mode-disable()
  "Disable the debugger track-mode hook"
  (interactive "")
  (if realgud-track-mode
      (setq realgud-track-mode nil)
    (message "Debugger is not in track mode")))

(defun realgud-track-mode-enable()
  "Enable the debugger track-mode hook"
  (interactive "")
  (if realgud-track-mode
      (message "Debugger track mode is already enabled.")
    (setq realgud-track-mode t))
  )

(provide-me "realgud-")
