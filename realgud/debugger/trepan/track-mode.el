;; Copyright (C) 2010, 2012-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Ruby "trepan" Debugger tracking a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepan-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-goto-line-for-pt 'realgud-track-mode)

(realgud-track-mode-vars "trepan")

(define-key realgud-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key realgud-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(declare-function realgud:ruby-populate-command-keys 'realgud-lang-ruby)

(defun realgud:trepan-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "control-frame"))

(defun realgud:trepan-goto-syntax-error-line (pt)
  "Display the location mentioned in a Syntax error line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "syntax-error"))

(realgud:ruby-populate-command-keys trepan-track-mode-map)

(define-key trepan-track-mode-map
  (kbd "C-c !c") 'realgud:trepan-goto-control-frame-line)
(define-key trepan-track-mode-map
  (kbd "C-c !s") 'realgud:trepan-goto-syntax-error-line)

(defun trepan-track-mode-hook()
  (if trepan-track-mode
      (progn
	(use-local-map trepan-track-mode-map)
	(message "using trepan mode map")
	)
    (message "trepan track-mode-hook disable called"))
)

(define-minor-mode trepan-track-mode
  "Minor mode for tracking trepan source locations inside a process shell via realgud. trepan is a Ruby debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepan-track-mode-map}
"
  :init-value nil
  ;; :lighter " trepan"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepan
  :keymap trepan-track-mode-map
  (realgud:track-set-debugger "trepan")
  (if trepan-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (trepan-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:trepan-")
