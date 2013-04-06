;; Copyright (C) 2010, 2012-2013 Rocky Bernstein <rocky@gnu.org>
;;
;; Python "trepan2" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud-trepan2-")

(realgud-track-mode-vars "trepan2")

(declare-function realgud-track-mode(bool))

(realgud-python-populate-command-keys trepan2-track-mode-map)

(defun trepan2-track-mode-hook()
  (if trepan2-track-mode
      (progn
	(use-local-map trepan2-track-mode-map)
	(message "using trepan2 mode map")
	)
    (message "trepan2 track-mode-hook disable called")
    )
)

(define-minor-mode trepan2-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepan2"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan2
  :keymap trepan2-track-mode-map
  (realgud-track-set-debugger "trepan2")
  (if trepan2-track-mode
      (progn
	(setq realgud-track-mode 't)
	(realgud-track-mode-setup 't)
	(run-mode-hooks (intern (trepan2-track-mode-hook))))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-trepan2-")
