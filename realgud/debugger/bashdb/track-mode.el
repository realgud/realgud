;;; Copyright (C) 2012-2014 Rocky Bernstein <rocky@gnu.org>
;;; Bash Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:bashdb-")
(require-relative "../../lang/posix-shell" nil "realgud-lang-")

(declare-function realgud-cmd-remap          'realgud-cmds)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud-posix-shell-populate-command-keys
		  'realgud-lang-posix-shell)

(realgud-track-mode-vars "bashdb")
(realgud-posix-shell-populate-command-keys bashdb-track-mode-map)

(declare-function realgud-track-mode(bool))

(defun bashdb-track-mode-hook()
  (if bashdb-track-mode
      (progn
	(use-local-map bashdb-track-mode-map)
	(message "using bashdb mode map")
	)
    (message "bashdb track-mode-hook disable called"))
)

(define-minor-mode bashdb-track-mode
  "Minor mode for tracking bashdb source locations inside a process shell via realgud. bashdb is a Bash debugger. See URL `http://bashdb.sf.net'.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{bashdb-track-mode-map}"
  :init-value nil
  ;; :lighter " bashdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:bashdb
  :keymap bashdb-track-mode-map

  (realgud:track-set-debugger "bashdb")
  (if bashdb-track-mode
      (progn
        (realgud-track-mode-setup 't)
        (bashdb-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(define-key bashdb-short-key-mode-map "T" 'realgud-cmd-backtrace)

(provide-me "realgud:bashdb-")
