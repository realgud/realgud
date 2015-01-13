;;; Copyright (C) 2012, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; GNU Make Debugger tracking a comint buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:remake-")

(declare-function realgud:cmd-remap          'realgud-cmds)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(realgud-track-mode-vars "remake")

(define-key remake-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key remake-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(defun remake-track-mode-hook()
  (if remake-track-mode
      (progn
	(use-local-map remake-track-mode-map)
	(message "using remake mode map")
	)
    (message "remake track-mode-hook disable called"))
)

(define-minor-mode remake-track-mode
  "Minor mode for tracking remake source locations inside a process shell via realgud. remake is a GNU Make debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{remake-track-mode-map}
"
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " remake"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:remake
  :keymap remake-track-mode-map

  (realgud:track-set-debugger "remake")
  (if remake-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (remake-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(define-key remake-short-key-mode-map "T" 'realgud:cmd-backtrace)

(provide-me "realgud:remake-")
