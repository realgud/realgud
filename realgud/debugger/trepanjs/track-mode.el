;;; Copyright (C) 2010, 2012-2015 Rocky Bernstein <rocky@gnu.org>
;;; Ruby "trepanjs" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepanjs-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-goto-line-for-pt 'realgud-track-mode)

(realgud-track-mode-vars "trepanjs")

(define-key realgud-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key realgud-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(declare-function realgud:ruby-populate-command-keys 'realgud-lang-ruby)

(defun realgud:trepanjs-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "control-frame"))

(defun realgud:trepanjs-goto-syntax-error-line (pt)
  "Display the location mentioned in a Syntax error line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "syntax-error"))

(realgud:ruby-populate-command-keys trepanjs-track-mode-map)

(define-key trepanjs-track-mode-map
  (kbd "C-c !c") 'realgud:trepanjs-goto-control-frame-line)
(define-key trepanjs-track-mode-map
  (kbd "C-c !s") 'realgud:trepanjs-goto-syntax-error-line)

(defun trepanjs-track-mode-hook()
  (if trepanjs-track-mode
      (progn
	(use-local-map trepanjs-track-mode-map)
	(message "using trepanjs mode map")
	)
    (message "trepanjs track-mode-hook disable called"))
)

(define-minor-mode trepanjs-track-mode
  "Minor mode for tracking trepanjs source locations inside a process shell via realgud. trepanjs is a Ruby debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepanjs-track-mode-map}
"
  :init-value nil
  ;; :lighter " trepanjs"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepanjs
  :keymap trepanjs-track-mode-map
  (realgud:track-set-debugger "trepanjs")
  (if trepanjs-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (trepanjs-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:trepanjs-")
