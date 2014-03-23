;; Copyright (C) 2010, 2012-2014 Rocky Bernstein <rocky@gnu.org>
;;
;; Python "trepan2" Debugger tracking a comint buffer.

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
(require-relative-list '("../../lang/python") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud-track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)

(realgud-track-mode-vars "trepan2")

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
  "Minor mode for tracking trepan2 source locations inside a process shell via realgud. trepan2 is a Python debugger. See URL `https://code.google.com/p/pydbgr/'.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepan2-track-mode-map}
"
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
