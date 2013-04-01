;;; Copyright (C) 2012 Rocky Bernstein <rocky@gnu.org>
;;; GNU Make Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud-remake-")

(realgud-track-mode-vars "remake")

(declare-function realgud-track-mode(bool))

(define-key remake-track-mode-map
  (kbd "C-c !!") 'realgud-goto-lang-backtrace-line)
(define-key remake-track-mode-map
  (kbd "C-c !b") 'realgud-goto-debugger-backtrace-line)

(defun remake-track-mode-hook()
  (if remake-track-mode
      (progn
	(use-local-map remake-track-mode-map)
	(message "using remake mode map")
	)
    (message "remake track-mode-hook disable called"))
)

(define-minor-mode remake-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " remake"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'remake
  :keymap remake-track-mode-map

  (realgud-track-set-debugger "remake")
  (if remake-track-mode
      (progn
	(setq realgud-track-mode 't)
        (realgud-track-mode-setup 't)
        (remake-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-remake-")
