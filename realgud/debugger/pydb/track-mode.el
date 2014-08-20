;;; Copyright (C) 2010, 2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;; Python "pydb" Debugger tracking a comint
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
(require-relative-list '("core" "init") "realgud:pydb-")

(realgud-track-mode-vars "pydb")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)

(realgud-python-populate-command-keys pydb-track-mode-map)

(defun pydb-track-mode-hook()
  (if pydb-track-mode
      (progn
        (use-local-map pydb-track-mode-map)
        (message "using pydb mode map")
        )
    (message "pydb track-mode-hook disable called")
    )
)

(define-minor-mode pydb-track-mode
  "Minor mode for tracking pydb source locations inside a process shell via realgud. pydb is a Python debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{pydb-track-mode-map}
"
  :init-value nil
  ;; :lighter " pydb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:pydb
  :keymap pydb-track-mode-map
  (realgud:track-set-debugger "pydb")
  (if pydb-track-mode
      (progn
        (setq realgud-track-mode 't)
        (realgud-track-mode-setup 't)
        (pydb-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:pydb-")
