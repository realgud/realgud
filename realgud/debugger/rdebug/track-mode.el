;; Copyright (C) 2010, 2012, 2015-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Ruby "rdebug" Debugger tracking a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud-rdebug-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(realgud-track-mode-vars "rdebug")

(declare-function realgud:ruby-populate-command-keys 'realgud-track-lang-ruby)
(declare-function realgud-track-mode                 'realgud-track-mode)
(declare-function realgud-track-mode-hook            'realgud-track-mode)
(declare-function realgud-track-mode-setup           'realgud-track-mode)
(declare-function realgud:track-set-debugger         'realgud-track-mode)

(realgud:ruby-populate-command-keys rdebug-track-mode-map)

(defun rdebug-track-mode-hook()
  (if rdebug-track-mode
      (progn
	(use-local-map rdebug-track-mode-map)
	(message "using rdebug mode map")
	)
    (message "rdebug track-mode-hook disable called"))
)

(define-minor-mode rdebug-track-mode
  "Minor mode for tracking rdebug source locations inside a process shell via realgud. rdebug is a Ruby debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{rdebug-track-mode-map}
"
  :init-value nil
  ;; :lighter " rdebug"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:rdebug
  :keymap rdebug-track-mode-map
  (rdebug-track-mode-internal rdebug-track-mode)
)

;; Broken out as a function for debugging
(defun rdebug-track-mode-internal (&optional arg)
  (realgud:track-set-debugger "rdebug")
  (if rdebug-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(rdebug-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-rdebug-")
