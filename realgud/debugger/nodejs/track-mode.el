;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
;;; nodejs tracking a comint buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud-nodejs-")
(require-relative "../../lang/posix-shell" nil "realgud-lang-")

(declare-function realgud-track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud-posix-shell-populate-command-keys
		  'realgud-lang-posix-shell)

(realgud-track-mode-vars "nodejs")
(realgud-posix-shell-populate-command-keys nodejs-track-mode-map)

(declare-function realgud-track-mode(bool))

(defun nodejs-track-mode-hook()
  (if nodejs-track-mode
      (progn
	(use-local-map nodejs-track-mode-map)
	(message "using nodejs mode map")
	)
    (message "nodejs track-mode-hook disable called"))
)

(define-minor-mode nodejs-track-mode
  "Minor mode for tracking nodejs source locations inside a process shell via realgud. nodejs is a Bash debugger. See URL `http://nodejs.sf.net'.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{nodejs-track-mode-map}"
  :init-value nil
  ;; :lighter " nodejs"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'nodejs
  :keymap nodejs-track-mode-map

  (realgud-track-set-debugger "nodejs")
  (if nodejs-track-mode
      (progn
        (realgud-track-mode-setup 't)
        (nodejs-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-nodejs-")
