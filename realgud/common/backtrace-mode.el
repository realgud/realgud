;;; Debugger Backtrace buffer mode settings
;;; Copyright (C) 2011, 2013 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list  '("menu" "key") "realgud-")
(require-relative-list  '("buffer/command") "realgud-buffer-")

(declare-function realgud-populate-debugger-menu 'realgud-menu)
(declare-function realgud-populate-common-keys 'realgud-menu)
(declare-function realgud-cmdbuf-pat 'realgud-menu)

(defvar realgud-backtrace-mode-map
  (let ((map  (realgud-populate-debugger-menu (make-sparse-keymap))))
    (suppress-keymap map)
    (realgud-populate-common-keys map)
    (define-key map "."       'realgud-backtrace-moveto-frame-selected)
    (define-key map "r"       'realgud:backtrace-init)
    (define-key map [double-mouse-1] 'realgud-goto-frame-mouse)
    (define-key map [mouse-2] 'realgud-goto-frame-mouse)
    (define-key map [mouse-3] 'realgud-goto-frame-mouse)
    (define-key map [up]      'realgud-backtrace-moveto-frame-prev)
    (define-key map [down]    'realgud-backtrace-moveto-frame-next)
    (define-key map "l"       'realgud-recenter-arrow)

    ;; FIXME: these can go to a common routine. See also shortkey.el and
    ;; key.el
    (define-key map "<"       'realgud-cmd-newer-frame)
    (define-key map ">"       'realgud-cmd-older-frame)
    (define-key map "d"       'realgud-cmd-newer-frame)
    (define-key map "u"       'realgud-cmd-older-frame)
    (define-key map "C"       'realgud-window-cmd-undisturb-src)
    (define-key map "F"       'realgud-window-bt)
    (define-key map "S"       'realgud-window-src-undisturb-cmd)

    (define-key map "n"       'realgud-backtrace-moveto-frame-next)
    (define-key map "p"       'realgud-backtrace-moveto-frame-prev)
    (define-key map "0"       'realgud-goto-frame-n)
    (define-key map "1"       'realgud-goto-frame-n)
    (define-key map "2"       'realgud-goto-frame-n)
    (define-key map "3"       'realgud-goto-frame-n)
    (define-key map "4"       'realgud-goto-frame-n)
    (define-key map "5"       'realgud-goto-frame-n)
    (define-key map "6"       'realgud-goto-frame-n)
    (define-key map "7"       'realgud-goto-frame-n)
    (define-key map "8"       'realgud-goto-frame-n)
    (define-key map "9"       'realgud-goto-frame-n)
    (define-key map [(control m)] 'realgud-goto-frame)

    ;; ;; --------------------
    ;; ;; The "Stack window" submenu.
    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key-after map [menu-bar debugger stack]
    ;;     (cons "Stack window" submenu)
    ;;     'placeholder))

    ;; (define-key map [menu-bar debugger stack goto]
    ;;   '(menu-item "Goto frame" realgud-goto-frame))
    map)
  "Keymap to navigate dbgr stack frames.")

(defun realgud-backtrace-mode (&optional cmdbuf)
  "Major mode for displaying the stack frames.
\\{realgud-frames-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'realgud-backtrace-mode)
  (setq mode-name "dbgr Stack Frames")
  ;; (set (make-local-variable 'realgud-secondary-buffer) t)
  (setq mode-line-process 'realgud-mode-line-process)
  (use-local-map realgud-backtrace-mode-map)

  ;; FIXME: make buffer specific
  (if cmdbuf
      (let* ((font-lock-keywords
	      (with-current-buffer cmdbuf
		(realgud-cmdbuf-pat "font-lock-keywords"))))
	(if font-lock-keywords
	    (set (make-local-variable 'font-lock-defaults)
		 (list font-lock-keywords)))
	))
  ;; (run-mode-hooks 'realgud-backtrace-mode-hook)
  )

(provide-me "realgud-")
