;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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
;; Python "pdb" Debugger tracking a comint buffer.

(require 'load-relative)
(require-relative-list '(
                         "../../common/cmds"
                         "../../common/menu"
                         "../../common/track"
                         "../../common/track-mode"
                         )
                       "realgud-")
(require-relative-list '("core" "init") "realgud:pdb-")

(realgud-track-mode-vars "pdb")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)

(realgud-python-populate-command-keys pdb-track-mode-map)

(defun pdb-track-mode-hook()
  (if pdb-track-mode
      (progn
        (use-local-map pdb-track-mode-map)
        (message "using pdb mode map")
        )
    (message "pdb track-mode-hook disable called")
    )
)

(define-minor-mode pdb-track-mode
  "Minor mode for tracking pdb source locations inside a process shell via realgud. pdb is the stock Python debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

a process shell.

\\{pdb-track-mode-map}
"
  :init-value nil
  ;; :lighter " pdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:pdb
  :keymap pdb-track-mode-map
  (realgud:track-set-debugger "pdb")
  (if pdb-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (pdb-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:pdb-")
