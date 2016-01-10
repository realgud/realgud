;;; Copyright (C) 2010-2015 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list '("custom" "eval" "helper" "key" "lochist" "loc"
			 "menu")
		       "realgud-")
(require-relative-list '("buffer/command" "buffer/helper" "buffer/source")
		       "realgud-buffer-")

(eval-when-compile
  (defvar realgud:tool-bar-map) ;; Fully defined in track-mode
)


(declare-function realgud-cmdbuf?                       'realgud-buffer-command)
(declare-function realgud:debugger-name-transform       'realgud-helper)
(declare-function realgud-get-cmdbuf                    'realgud-buffer-helper)
(declare-function realgud:loc-follow                    'realgud-loc)
(declare-function realgud-loc-hist-item-at              'realgud-lochist)
(declare-function realgud-cmdbuf-loc-hist               'realgud-command)
(declare-function realgud-populate-debugger-menu        'realgud-menu)
(declare-function realgud-populate-common-keys          'realgud-key)
(declare-function realgud-populate-src-buffer-map-plain 'realgud-key)
(declare-function realgud-srcbuf-info-short-key?=,      'realgud-source)
(declare-function realgud-srcbuf-info-was-read-only?=   'realgud-source)
(declare-function realgud-srcbuf?                       'realgud-buffer-source)

;; (defvar realgud::tool-bar-map) ;; fully defined in track-mode.el

(defvar realgud:shortkey-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (realgud-populate-debugger-menu map)
    (realgud-populate-common-keys map)
    (realgud-populate-src-buffer-map-plain map)
    (define-key map "1"        'realgud-goto-arrow1)
    (define-key map "2"        'realgud-goto-arrow2)
    (define-key map "3"        'realgud-goto-arrow3)
    (define-key map "4"        'realgud:goto-loc-hist-4)
    (define-key map "5"        'realgud:goto-loc-hist-5)
    (define-key map "6"        'realgud:goto-loc-hist-6)
    (define-key map "7"        'realgud:goto-loc-hist-7)
    (define-key map "8"        'realgud:goto-loc-hist-8)
    (define-key map "9"        'realgud:goto-loc-hist-9)
    (define-key map "b"        'realgud:cmd-break)
    (define-key map "c"        'realgud:cmd-continue)
    (define-key map "e"        'realgud:cmd-eval-region)
    (define-key map "U"        'realgud:cmd-until)
    (define-key map [mouse-2]  'realgud:tooltip-eval)

    ;; FIXME: these can go to a common routine
    (define-key map "<"        'realgud:cmd-newer-frame)
    (define-key map ">"        'realgud:cmd-older-frame)
    (define-key map "d"        'realgud:cmd-newer-frame)
    (define-key map "u"        'realgud:cmd-older-frame)
    (define-key map "l"        'realgud-recenter-arrow)
    (define-key map "C"        'realgud-window-cmd-undisturb-src)
    (define-key map "I"        'realgud:cmdbuf-info-describe)
    (define-key map "S"        'realgud-window-src-undisturb-cmd)

    (define-key map "R"        'realgud:cmd-restart)
    (define-key map "!"        'realgud:cmd-shell)
    (define-key map [insert]   'realgud-short-key-mode)
    (define-key map [(control x)(control q)] 'realgud-short-key-mode)
    map)
  "Keymap used in `realgud-short-key-mode'.")

;; Implementation note: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode realgud-short-key-mode
  "Minor mode with short keys for source buffers for the `dbgr' debugger.
The buffer is read-only when the minor mode is active.

\\{realgud:shortkey-mode-map}"
  :group 'realgud
  :global nil
  :init-value nil
  :lighter " ShortKeys"
  :keymap realgud:shortkey-mode-map
  ;; executed on activation/deactivation:
  (realgud-short-key-mode-setup realgud-short-key-mode))

(defun realgud-get-short-key-mode-map (cmdbuf)
  (when (realgud-cmdbuf? cmdbuf)
    (with-current-buffer cmdbuf
      (let* ((info realgud-cmdbuf-info)
	     (debugger-name (realgud-cmdbuf-info-debugger-name info))
	     (base-variable-name
	      (or (gethash debugger-name realgud:variable-basename-hash)
	      debugger-name))
	     (keymap-symbol
	      (intern
	       (replace-regexp-in-string
		"\\." ""
		(concat base-variable-name "-short-key-mode-map"))))
	     (keymap (eval keymap-symbol))
	     )
	(cond ((keymapp keymap) keymap)
	      ('t nil))
	))
    ))

(defun realgud-short-key-mode-setup (mode-on?)
  "Called when entering or leaving `realgud-short-key-mode'. Variable
MODE-ON? a boolean which specifies if we are going into or out of this mode."
  (if (realgud-srcbuf?)
    (let* ((cmdbuf (realgud-get-cmdbuf))
	   (shortkey-keymap (realgud-get-short-key-mode-map cmdbuf))
	   )

      ;; If there's a shortkey keymap that is custom
      ;; for this debugger mode, use it.
      (when shortkey-keymap
	(cond
	 (mode-on?
	  (set (make-local-variable 'tool-bar-map) realgud:tool-bar-map)
	  (use-local-map shortkey-keymap))
	 ('t
	  (kill-local-variable 'realgud:tool-bar-map)
	  (use-local-map nil))
	  ))

      ;; Ensure action only is performed when the state actually is toggled.
      ;; or when not read-only
      (when (or (not buffer-read-only)
		(not (eq (realgud-sget 'srcbuf-info 'short-key?) mode-on?)))
	;; Save the current state, so we can determine when the
	;; state is toggled in the future.
	(when (not (eq (realgud-sget 'srcbuf-info 'short-key?) mode-on?))
	  (realgud-srcbuf-info-short-key?= mode-on?)
	  (setq realgud-short-key-mode mode-on?)
	  (if mode-on?
	      ;; mode is being turned on.
	      (progn
		(realgud-srcbuf-info-was-read-only?= buffer-read-only)

		;; If there's a shortkey keymap that is custom
		;; for this debugger mode, use it.
		(if shortkey-keymap (use-local-map shortkey-keymap))

		(local-set-key [m-insert] 'realgud-short-key-mode)
		(when realgud-srcbuf-lock (setq buffer-read-only t))
		(run-mode-hooks 'realgud-short-key-mode-hook))
	    ;; mode is being turned off: restore read-only state.
	    (setq buffer-read-only
		  (realgud-sget 'srcbuf-info 'was-read-only?))))
    ;; (with-current-buffer-safe cmdbuf
    ;;   (realgud-cmdbuf-info-src-shortkey?= mode-on?)
    ;;   (realgud-cmdbuf-info-in-srcbuf?= mode-on?)
    ;;   )
    ))
    (progn
      (setq realgud-short-key-mode nil)
      (error "buffer %s does not seem to be attached to a debugger"
	   (buffer-name)))))

(defun realgud-short-key-mode-off ()
  "Turn off `realgud-short-key-mode' in all buffers."
  (interactive)
  (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when realgud-short-key-mode
	  (realgud-short-key-mode-setup 0)))))

(defun realgud-populate-src-buffer-map (map)
  "Bind all common keys and menu used in the dbgr src buffers.
This includes the keys bound to `realgud-key-prefix' (typically C-x
C-a)."
  (realgud-populate-src-buffer-map-plain map)
  (realgud-populate-common-keys map)
  (let ((prefix-map (make-sparse-keymap)))
    (realgud-populate-debugger-menu map)
    (realgud-populate-src-buffer-map-plain prefix-map)
    (define-key map realgud-key-prefix prefix-map)))

(defun realgud:goto-loc-hist(num)
  "Go to position nth from the newest position."
  (let ((cmdbuf (realgud-get-cmdbuf)))
    (if cmdbuf
      (let* ((loc-hist (realgud-cmdbuf-loc-hist cmdbuf))
	    (loc (realgud-loc-hist-item-at loc-hist (- num)))
	    (loc-marker (realgud-loc-marker loc)))
	(realgud:loc-follow loc-marker))
      ;; else
	(message "No command buffer associated with this buffer")
    )))


(defun realgud:goto-loc-hist-4 ()
  "Go to position 4th from the newest position."
  (interactive "")
  (realgud:goto-loc-hist 4))

(defun realgud:goto-loc-hist-5 ()
  "Go to position 5th from the newest position."
  (interactive "")
  (realgud:goto-loc-hist 5))

(defun realgud:goto-loc-hist-6 ()
  (interactive "")
  (realgud:goto-loc-hist 6))

(defun realgud:goto-loc-hist-7 ()
  "Go to position 7th from the newest position."
  (interactive "")
  (realgud:goto-loc-hist 7))

(defun realgud:goto-loc-hist-8 ()
  "Go to position 8th from the newest position."
  (interactive "")
  (realgud:goto-loc-hist 8))

(defun realgud:goto-loc-hist-9 ()
  "Go to position 9th from the newest position."
  (interactive "")
  (realgud:goto-loc-hist 9))

(provide-me "realgud-")

;;; Local variables:
;;; End:
