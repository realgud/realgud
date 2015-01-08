;; Copyright (C) 2010, 2012-2015 Rocky Bernstein <rocky@gnu.org>
;;
;; Python "pydbgr" Debugger tracking a comint buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")

(require-relative-list '("../../lang/python") "realgud-lang-")
(require-relative-list '("core" "init") "realgud-pydbgr-")

(realgud-track-mode-vars "pydbgr")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)

(realgud-python-populate-command-keys pydbgr-track-mode-map)

(defun pydbgr-track-mode-hook()
  (if pydbgr-track-mode
      (progn
	(use-local-map pydbgr-track-mode-map)
	(message "using pydbgr mode map")
	)
    (message "pydbgr track-mode-hook disable called")
    )
)

(define-minor-mode pydbgr-track-mode
  "Minor mode for tracking pydbgr source locations inside a process shelll via realgud. pydbgr is a Python debugger. See URL `https://code.google.com/p/pydbgr/'.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{pydbgr-track-mode-map}
"
  :init-value nil
  ;; :lighter " pydbgr"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:pydbgr
  :keymap pydbgr-track-mode-map
  (realgud:track-set-debugger "pydbgr")
  (if pydbgr-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(pydbgr-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-pydbgr-")
