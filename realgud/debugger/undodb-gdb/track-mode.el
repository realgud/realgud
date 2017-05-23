
;; Copyright (C) 2015-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Felipe Lema <felipelema@mortemale.org>

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

;; undodb-gdb tracking a comint or eshell buffer.

;;; Code:

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:undodb-gdb-")

(realgud-track-mode-vars "realgud:undodb-gdb")

(declare-function realgud-track-mode         'realgud-track-mode)
(declare-function realgud:track-mode-hook    'realgud-track-mode)
(declare-function realgud:track-mode-enable  'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(define-key realgud:undodb-gdb-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(defun realgud:undodb-gdb-track-mode-hook()
  (use-local-map realgud:undodb-gdb-track-mode-map)
  (realgud-track-mode-setup 't)
  (message "realgud:undodb-gdb track-mode-hook called")
  )

(define-minor-mode realgud:undodb-gdb-track-mode
  "Minor mode for tracking undodb-gdb inside a process shell via realgud.

If called interactively with no prefix argument, the mode is toggled. A prefix
argument, captured as ARG, enables the mode if the argument is positive, and
disables it otherwise.

Key bindings:
\\{realgud:undodb-gdb-track-mode-map}
"
  :init-value nil
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:undodb-gdb
  :keymap realgud:undodb-gdb-track-mode-map
  (if realgud:undodb-gdb-track-mode
      (progn
	(realgud:track-set-debugger "undodb-gdb")
        (realgud:undodb-gdb-track-mode-hook)
        (realgud:track-mode-enable))
    (progn
      (setq realgud-track-mode nil)
      ))
  )

(provide-me "realgud:undodb-gdb-")
;;; track-mode.el ends here
