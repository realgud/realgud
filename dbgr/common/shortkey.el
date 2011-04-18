;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list '("custom" "key" "menu") "dbgr-")
(require-relative-list '("buffer/helper") "dbgr-buffer-")

(defvar dbgr-short-key-mode-map 
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (dbgr-populate-common-keys map)
    (dbgr-populate-src-buffer-map-plain map)
    (dbgr-populate-debugger-menu map)
    (define-key map "1"        'dbgr-goto-arrow1)
    (define-key map "2"        'dbgr-goto-arrow2)
    (define-key map "3"        'dbgr-goto-arrow3)
    (define-key map "b"        'dbgr-cmd-break)
    (define-key map "c"        'dbgr-cmd-continue)
    (define-key map "e"        'dbgr-cmd-eval-region)

    ;; FIXME: these can go to a common routine
    (define-key map "<"        'dbgr-cmd-newer-frame)
    (define-key map ">"        'dbgr-cmd-older-frame)
    (define-key map "d"        'dbgr-cmd-newer-frame)
    (define-key map "u"        'dbgr-cmd-older-frame)
    (define-key map "l"        'dbgr-recenter-arrow)
    (define-key map "B"        'dbgr-backtrace-init)
    (define-key map "C"        'dbgr-window-cmd-undisturb-src)
    (define-key map "S"        'dbgr-window-src-undisturb-cmd)

    (define-key map "R"        'dbgr-cmd-restart)
    (define-key map "!"        'dbgr-cmd-shell)
    (define-key map [insert]   'dbgr-short-key-mode)
    (define-key map [M-insert] 'dbgr-short-key-mode)
    map)
  "Keymap used in `dbgr-short-key-mode'.")


;; Implementation note: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode dbgr-short-key-mode
  "Minor mode with short keys for source buffers for the `dbgr' debugger.
The buffer is read-only when the minor mode is active.

\\{dbgr-short-key-mode-map}"
  :group 'dbgr
  :global nil
  :init-value nil
  :lighter " ShortKeys"
  :keymap dbgr-short-key-mode-map
  ;; executed on activation/deactivation:
  (dbgr-short-key-mode-setup dbgr-short-key-mode))

(defun dbgr-short-key-mode-setup (mode-on?)
  "Called when entering or leaving `dbgr-short-key-mode'. Variable
MODE-ON? a boolean which specifies if we are going into or out of this mode."
  (if (dbgr-srcbuf?)
      (let ((cmdbuf (dbgr-get-cmdbuf)))
	;; Ensure action only is performed when the state actually is toggled.
	(unless (eq (dbgr-sget 'srcbuf-info 'short-key?) mode-on?)
	  (if mode-on?
	      ;; Mode is being turned on.
	      (progn
		(dbgr-srcbuf-info-was-read-only?= buffer-read-only)
		(local-set-key [M-insert] 'dbgr-short-key-mode)
		(setq buffer-read-only t))
	    ;; Mode is being turned off: restore read-only state.
	    (setq buffer-read-only (dbgr-sget 'srcbuf-info 'was-read-only?)))
	  ;; Save the current state, so we can determine when the
	  ;; state is toggled in the future.
	  (dbgr-srcbuf-info-short-key?= mode-on?)
	  (setq dbgr-short-key-mode mode-on?))
	;; (with-current-buffer-safe cmdbuf
	;;   (dbgr-cmdbuf-info-src-shortkey?= mode-on?)
	;;   (dbgr-cmdbuf-info-in-srcbuf?= mode-on?)
	;;   )
	)
    (error "Buffer %s does not seem to be attached to a debugger" 
	   (buffer-name))))

(defun dbgr-short-key-mode-off ()
  "Turn off `dbgr-short-key-mode' in all buffers."
  (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when dbgr-short-key-mode
	  (dbgr-short-key-mode-setup 0)))))

(defun dbgr-populate-src-buffer-map (map)
  "Bind all common keys and menu used in the dbgr src buffers.
This includes the keys bound to `dbgr-key-prefix' (typically C-x
C-a)."
  (dbgr-populate-src-buffer-map-plain map)
  (dbgr-populate-common-keys map)
  (dbgr-populate-debugger-menu map)
  (let ((prefix-map (make-sparse-keymap)))
    (dbgr-populate-src-buffer-map-plain prefix-map)
    (define-key map dbgr-key-prefix prefix-map)))

(provide-me "dbgr-")

;;; Local variables:
;;; End:
