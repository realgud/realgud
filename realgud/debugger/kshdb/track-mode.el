;;; Copyright (C) 2012, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; "kshdb" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:kshdb-")

(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud-posix-shell-populate-command-keys
		  'realgud-lang-posix-shell)

(realgud-track-mode-vars "kshdb")
(realgud-posix-shell-populate-command-keys kshdb-track-mode-map)

(declare-function realgud-track-mode(bool))

(defun kshdb-track-mode-hook()
  (if kshdb-track-mode
      (progn
	(use-local-map kshdb-track-mode-map)
	(message "using kshdb mode map")
	)
    (message "kshdb track-mode-hook disable called"))
)

(define-minor-mode kshdb-track-mode
  "Minor mode for tracking kshdb source locations inside a process shell via realgud. kshdb is a Korn Shell debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{kshdb-track-mode-map}
"
  :init-value nil
  ;; :lighter " kshdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'kshdb
  :keymap kshdb-track-mode-map

  (realgud:track-set-debugger "kshdb")
  (if kshdb-track-mode
      (progn
	(realgud-track-mode 't)
        (kshdb-track-mode-hook))
    (progn
      (realgud-track-mode nil)
      ))
)

(provide-me "realgud:kshdb-")
