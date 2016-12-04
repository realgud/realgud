;; Copyright (C) 2013-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Golang SSA gub tracking a comint buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:gub-")

(realgud-track-mode-vars "gub")

(declare-function realgud-goto-line-for-pt 'realgud-track-mode)
(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)

(defun realgud:gub-goto-location (pt)
  "Display the location mentioned in a location
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "general-location"))


(defun realgud:gub-goto-panic-location (pt)
  "Display the location mentioned in a location
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "panic-backtrace"))


(define-key gub-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key gub-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
(define-key gub-track-mode-map
  (kbd "C-c !s") 'realgud:gub-goto-location)
(define-key gub-track-mode-map
  (kbd "C-c !p") 'realgud:gub-goto-panic-location)

(defun gub-track-mode-hook()
  (if gub-track-mode
      (progn
	(use-local-map gub-track-mode-map)
	(message "using gub mode map")
	)
    (message "gub track-mode-hook disable called"))
)

(define-minor-mode gub-track-mode
  "Minor mode for tracking gub source locations inside a process shell via realgud. gub is a Go language debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{gub-track-mode-map}
"
  :init-value nil
  ;; :lighter " gub"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:gub
  :keymap gub-track-mode-map

  (realgud:track-set-debugger "gub")
  (if gub-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (gub-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:gub-")
