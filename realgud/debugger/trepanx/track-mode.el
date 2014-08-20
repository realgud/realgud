;;; Copyright (C) 2010, 2012-2014 Rocky Bernstein <rocky@gnu.org>
;;; Ruby "trepanx" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepanx-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(realgud-track-mode-vars "trepanx")

(declare-function realgud-track-mode(bool))

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-ruby-populate-command-keys 'realgud-lang-ruby)

(realgud-ruby-populate-command-keys trepanx-track-mode-map)

(define-key trepanx-track-mode-map
  (kbd "C-c !x") 'realgud:rubinius-goto-Xagent-backtrace-line)
(define-key trepanx-track-mode-map
  (kbd "C-c !!") 'realgud:ruby-goto-dollar-bang-line)

(defun trepanx-track-mode-hook()
  (use-local-map trepanx-track-mode-map)
  (message "trepanx track-mode-hook called")
)
(define-minor-mode trepanx-track-mode
  "Minor mode for tracking trepanx source locations inside a process shell via realgud. trepanx is a Rubinius Ruby debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepanx-track-mode-map}"

  :init-value nil
  ;; :lighter " trepanx"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepanx
  :keymap trepanx-track-mode-map

  (realgud:track-set-debugger "trepanx")
  (if trepanx-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(trepanx-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:trepanx-")
