;; Copyright (C) 2010-2012, 2014-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Python "trepan2" Debugger tracking in a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepan2-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)

(realgud-track-mode-vars "trepan2")

(realgud-python-populate-command-keys trepan2-track-mode-map)

(defun trepan2-track-mode-hook()
  (if trepan2-track-mode
      (progn
	(use-local-map trepan2-track-mode-map)
	(message "using trepan2 mode map")
	)
    (message "trepan2 track-mode-hook disable called")
    )
)

(define-minor-mode trepan2-track-mode
  "Minor mode for tracking trepan2 source locations inside a process shell via realgud. trepan2 is a Python debugger. See URL `https://github.com/rocky/emacs-dbgr/rocky/python2-trepan/'.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepan2-track-mode-map}
"
  :init-value nil
  ;; :lighter " trepan2"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepan2
  :keymap trepan2-track-mode-map

  (realgud:track-set-debugger "trepan2")
  (if trepan2-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (trepan2-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(define-key trepan2-short-key-mode-map "T" 'realgud:cmd-backtrace)

(provide-me "realgud:trepan2-")
