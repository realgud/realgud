;;  tracks shell output 

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list
 '("core" "helper" "track" "loc" "lochist" "file" 
   "fringe" "cmdbuf" "window" "regexp" "menu"
   "send" "shortkey") "dbgr-")

(defvar dbgr-track-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right]	'dbgr-track-hist-newest)
    (define-key map [M-down]	'dbgr-track-hist-newer)
    (define-key map [M-up]	'dbgr-track-hist-older)
    (define-key map [M-print]	'dbgr-track-hist-older)
    (define-key map [M-S-down]	'dbgr-track-hist-newest)
    (define-key map [M-S-up]	'dbgr-track-hist-oldest)
    (dbgr-populate-debugger-menu map)
    map)
  "Keymap used in `dbgr-track-minor-mode'.")

;; FIXME figure out if I can put this in something like a header file.
(declare-function dbgr-track-set-debugger (debugger-name &optional hash))

(define-minor-mode dbgr-track-mode
  "Minor mode for tracking debugging inside a process shell."
  :init-value nil
  :global nil
  :group 'dbgr

  :lighter 
  (:eval (progn 
	   (concat " "
		   (if (dbgr-cmdbuf-info-set?)
		       (dbgr-sget 'cmdbuf-info 'debugger-name)
		     "dbgr??"))))

  :keymap dbgr-track-mode-map
  ;; Setup/teardown
  (dbgr-track-mode-setup dbgr-track-mode)
  )

(defun dbgr-track-mode-setup (mode-on?)
  "Called when entering or leaving `dbgr-track-mode'. Variable
MODE-ON is a boolean which specifies if we are going into or out
of this mode."
  (if mode-on?
      (let ((process (get-buffer-process (current-buffer))))
	(unless process
	  (setq dbgr-track-mode nil)
	  (error "Can't find a process for buffer %s" (current-buffer)))

	;; FIXME: save and chain process-sentinel via
	;; (process-sentinel (get-buffer-process (current-buffer)))
	(set-process-sentinel process 'dbgr-term-sentinel)
	(unless (and (dbgr-cmdbuf-info-set?)
		     (dbgr-sget 'cmdbuf-info 'debugger-name))
	  (call-interactively 'dbgr-track-set-debugger))
	(if (boundp 'comint-last-output-start)
	    (progn
	      (dbgr-cmdbuf-info-prior-prompt-regexp= 
	       dbgr-cmdbuf-info comint-prompt-regexp)
	      (let* ((regexp-hash
		      (and (dbgr-cmdbuf-info? dbgr-cmdbuf-info)
			   (dbgr-sget 'cmdbuf-info 'regexp-hash)))
		     (prompt-pat (and regexp-hash 
				      (gethash "prompt" regexp-hash))))
		(if prompt-pat
		    (setq comint-prompt-regexp 
			    (dbgr-loc-pat-regexp prompt-pat)))))
	  (set-marker comint-last-output-start (point)))

	(add-hook 'comint-output-filter-functions 
		  'dbgr-track-comint-output-filter-hook)
	(add-hook 'eshell-output-filter-functions 
		  'dbgr-track-eshell-output-filter-hook)
	(run-mode-hooks 'dbgr-track-mode-hook))
    (progn
      (if (boundp 'comint-last-output-start)
	(setq comint-prompt-regexp
	   (dbgr-sget 'cmdbuf-info 'prior-prompt-regexp))
	)
      (dbgr-fringe-erase-history-arrows)
      (remove-hook 'comint-output-filter-functions 
		   'dbgr-track-comint-output-filter-hook)
      (remove-hook 'eshell-output-filter-functions 
		    'dbgr-track-eshell-output-filter-hook)
      ;; FIXME: restore/unchain old process sentinels.
      )
    )
)

(defmacro dbgr-track-mode-vars (name)
  (list 'defvar 
	(intern (concat name "-track-mode")) nil
	(format "Non-nil if using %s-track-mode as a minor mode of some other mode.
Use the command `%s-track-mode' to toggle or set this variable." name name))
  (list 'defvar 
	(intern (concat name "-track-minor-mode-map")) '(make-sparse-keymap)
	(format "Keymap used in %s-track-minor-mode'." name)))

;; FIXME: The below could be a macro? I have a hard time getting
;; macros right.
(defun dbgr-track-mode-body(name)
  "Used in by custom debuggers: pydbgr, trepan, gdb, etc. NAME is
the name of the debugger which is used to preface variables."
  (dbgr-track-set-debugger name)
  (funcall (intern (concat "dbgr-define-" name "-commands")))
  (if (intern (concat name "-track-mode"))
      (progn 
	(dbgr-define-gdb-like-commands) ;; FIXME: unless already defined
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (concat name "-track-mode-hook"))))
    (progn 
      (dbgr-track-mode nil)
      )))

(provide-me "dbgr-")
