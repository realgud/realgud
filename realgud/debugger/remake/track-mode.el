;; Copyright (C) 2014-2021  Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; GNU Make Debugger tracking a comint buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:remake-")

(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(realgud-track-mode-vars "remake")

(define-key remake-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key remake-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(defun remake-track-mode-hook()
  (if remake-track-mode
      (progn
	(use-local-map remake-track-mode-map)
	(message "using remake mode map")
	)
    (message "remake track-mode-hook disable called"))
)

(define-minor-mode remake-track-mode
  "Minor mode for tracking remake source locations inside a process shell via realgud. remake is a GNU Make debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{remake-track-mode-map}"
  ;; :lighter " remake"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:remake

  (realgud:track-set-debugger "remake")
  (if remake-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (remake-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(define-key remake-short-key-mode-map "T" 'realgud:cmd-backtrace)

(provide-me "realgud:remake-")
