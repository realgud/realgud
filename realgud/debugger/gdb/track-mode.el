;;; Copyright (C) 2010, 2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;; gdb tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:gdb-")

(realgud-track-mode-vars "realgud:gdb")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud:track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(define-key realgud:gdb-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(defun realgud:gdb-track-mode-hook()
  (use-local-map realgud:gdb-track-mode-map)
  (message "realgud:gdb track-mode-hook called")
)

(define-minor-mode realgud:gdb-track-mode
  "Minor mode for tracking gdb inside a process shell via realgud.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

Key bindings:
\\{realgud:gdb-track-mode-map}
"
  :init-value nil
  ;; :lighter " gdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:gdb
  :keymap realgud:gdb-track-mode-map
  (if realgud:gdb-track-mode
      (progn
	(realgud:track-set-debugger "gdb")
        (realgud:gdb-track-mode-hook)
        (realgud:track-mode-enable))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:gdb-")
