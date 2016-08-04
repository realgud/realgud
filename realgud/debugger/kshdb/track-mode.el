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
;;; "kshdb" Debugger tracking a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:kshdb-")

(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud-posix-shell-populate-command-keys
		  'realgud-lang-posix-shell)

(realgud-track-mode-vars "kshdb")
(realgud-posix-shell-populate-command-keys kshdb-track-mode-map)

(declare-function realgud-track-mode(bool))

(defun kshdb-track-mode-hook()
  (if kshdb-track-mode
      (progn
	(use-local-map kshdb-track-mode-map)
	(message "using kshdb mode map")
	)
    (message "kshdb track-mode-hook disable called"))
)

(define-minor-mode kshdb-track-mode
  "Minor mode for tracking kshdb source locations inside a process shell via realgud. kshdb is a Korn Shell debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{kshdb-track-mode-map}
"
  :init-value nil
  ;; :lighter " kshdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'kshdb
  :keymap kshdb-track-mode-map

  (realgud:track-set-debugger "kshdb")
  (if kshdb-track-mode
      (progn
	(realgud-track-mode 't)
        (kshdb-track-mode-hook))
    (progn
      (realgud-track-mode nil)
      ))
)

(provide-me "realgud:kshdb-")
