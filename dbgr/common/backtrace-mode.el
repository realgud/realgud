;;; Debugger Backtrace buffer mode settings
;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list  '("menu") "dbgr-")
(defvar dbgr-backtrace-mode-map
  (let ((map  (dbgr-populate-debugger-menu (make-sparse-keymap))))
    (suppress-keymap map)
    (dbgr-populate-common-keys map)
    (define-key map "."       'dbgr-backtrace-moveto-frame-selected)
    (define-key map "r"       'dbgr-backtrace-init)
    (define-key map [double-mouse-1] 'dbgr-goto-frame-mouse)
    (define-key map [mouse-2] 'dbgr-goto-frame-mouse)
    (define-key map [mouse-3] 'dbgr-goto-frame-mouse)
    (define-key map [up]      'dbgr-backtrace-moveto-frame-prev)
    (define-key map [down]    'dbgr-backtrace-moveto-frame-next)
    (define-key map "l"       'dbgr-recenter-arrow)

    ;; FIXME: these can go to a common routine. See also shortkey.el and
    ;; key.el
    (define-key map "<"       'dbgr-cmd-newer-frame)
    (define-key map ">"       'dbgr-cmd-older-frame)
    (define-key map "d"       'dbgr-cmd-newer-frame)
    (define-key map "u"       'dbgr-cmd-older-frame)
    (define-key map "C"       'dbgr-window-cmd-undisturb-src)
    (define-key map "F"       'dbgr-window-bt)
    (define-key map "S"       'dbgr-window-src-undisturb-cmd)

    (define-key map "n"       'dbgr-backtrace-moveto-frame-next)
    (define-key map "p"       'dbgr-backtrace-moveto-frame-prev)
    (define-key map "0"       'dbgr-goto-frame-n)
    (define-key map "1"       'dbgr-goto-frame-n)
    (define-key map "2"       'dbgr-goto-frame-n)
    (define-key map "3"       'dbgr-goto-frame-n)
    (define-key map "4"       'dbgr-goto-frame-n)
    (define-key map "5"       'dbgr-goto-frame-n)
    (define-key map "6"       'dbgr-goto-frame-n)
    (define-key map "7"       'dbgr-goto-frame-n)
    (define-key map "8"       'dbgr-goto-frame-n)
    (define-key map "9"       'dbgr-goto-frame-n)
    (define-key map [(control m)] 'dbgr-goto-frame)

    ;; ;; --------------------
    ;; ;; The "Stack window" submenu.
    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key-after map [menu-bar debugger stack]
    ;;     (cons "Stack window" submenu)
    ;;     'placeholder))

    ;; (define-key map [menu-bar debugger stack goto]
    ;;   '(menu-item "Goto frame" dbgr-goto-frame))
    map)
  "Keymap to navigate dbgr stack frames.")

(defun dbgr-backtrace-mode (&optional cmdbuf)
  "Major mode for displaying the stack frames.
\\{dbgr-frames-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'dbgr-backtrace-mode)
  (setq mode-name "dbgr Stack Frames")
  ;; (set (make-local-variable 'dbgr-secondary-buffer) t)
  (setq mode-line-process 'dbgr-mode-line-process)
  (use-local-map dbgr-backtrace-mode-map)

  ;; FIXME: make buffer specific
  (if cmdbuf
      (let* ((font-lock-keywords 
	      (with-current-buffer cmdbuf
		(dbgr-cmdbuf-pat "font-lock-keywords"))))
	(if font-lock-keywords
	    (set (make-local-variable 'font-lock-defaults)
		 (list font-lock-keywords)))
	))
  ;; (run-mode-hooks 'dbgr-backtrace-mode-hook)
  )

(provide-me "dbgr-")
