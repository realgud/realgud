;; Copyright (C) 2012-2016 Free Software Foundation, Inc

;; Author: Free Software Foundation, Inc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Author: Rocky Bernstein <rocky@gnu.org>

;; Bash Debugger tracking in a comint or eshell buffer.

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

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-posix-shell-populate-command-keys
		  'realgud-lang-posix-shell)

(realgud-track-mode-vars "bashdb")

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

(define-key bashdb-short-key-mode-map "T" 'realgud:cmd-backtrace)

(provide-me "realgud:bashdb-")
