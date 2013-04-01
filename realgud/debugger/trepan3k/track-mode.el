;;; Copyright (C) 2010, 2012-2013 Rocky Bernstein <rocky@gnu.org>
;;; Python "trepan3k" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud-trepan3k-")

(realgud-track-mode-vars "trepan3k")

(declare-function realgud-track-mode(bool))

(realgud-python-populate-command-keys trepan3k-track-mode-map)

(defun trepan3k-track-mode-hook()
  (if trepan3k-track-mode
      (progn
	(use-local-map trepan3k-track-mode-map)
	(message "using trepan3k mode map")
	)
    (message "trepan3k track-mode-hook disable called")
    )
)

(define-minor-mode trepan3k-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepan3k"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan3k
  :keymap trepan3k-track-mode-map
  (realgud-track-set-debugger "trepan3k")
  (if trepan3k-track-mode
      (progn
	(setq realgud-track-mode 't)
	(realgud-track-mode-setup 't)
	(run-mode-hooks (intern (trepan3k-track-mode-hook))))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-trepan3k-")
