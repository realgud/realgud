;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Sean Farley <sean@farley.io>

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
;; Python "ipdb" Debugger tracking a comint buffer.

(require 'load-relative)
(require-relative-list '(
                         "../../common/cmds"
                         "../../common/menu"
                         "../../common/track"
                         "../../common/track-mode"
                         )
                       "realgud-")
(require-relative-list '("core" "init") "realgud:ipdb-")

(realgud-track-mode-vars "ipdb")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)
(declare-function realgud:ipdb-completion-at-point 'realgud:ipdb-core)

(realgud-python-populate-command-keys ipdb-track-mode-map)

(defun ipdb-track-mode-hook()
  (if ipdb-track-mode
      (progn
        (use-local-map ipdb-track-mode-map)
        (add-hook 'completion-at-point-functions
                  'realgud:ipdb-completion-at-point nil t)
        (message "using ipdb mode map")
        )
    (message "ipdb track-mode-hook disable called")
    )
)

(define-minor-mode ipdb-track-mode
  "Minor mode for tracking ipdb source locations inside a process shell via realgud. ipdb is a Python debugger based on ipython.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

a process shell.

\\{ipdb-track-mode-map}
"
  :init-value nil
  ;; :lighter " ipdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:ipdb
  :keymap ipdb-track-mode-map
  (realgud:track-set-debugger "ipdb")
  (if ipdb-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (ipdb-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:ipdb-")
