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
;; Mode for parsing various kinds of backtraces found in Java

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track-mode"
			 "../../common/backtrack-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:jdb-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(realgud-backtrack-mode-vars "jdb")
(set-keymap-parent jdb-backtrack-mode-map realgud-backtrack-mode-map)

(declare-function realgud-backtrack-mode         'realgud-common-backtrack-mode)
(declare-function realgud-backtrack-set-debugger 'realgud-common-backtrack-mode)
(declare-function realgud-goto-line-for-pt       'realgud-common-backtrack-mode)
(declare-function realgud:ruby-populate-command-keys 'realgud-lang-ruby)

(defun realgud:jdb-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "control-frame"))

(realgud:ruby-populate-command-keys jdb-backtrack-mode-map)
(define-key jdb-backtrack-mode-map
  (kbd "C-c !c") 'realgud:jdb-goto-control-frame-line)

(define-minor-mode jdb-backtrack-mode
  "Minor mode for tracking ruby debugging inside a file which may not have process shell.

\\{jdb-backtrack-mode-map}"
  :init-value nil
  ;; :lighter " jdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:jdb
  :keymap jdb-backtrack-mode-map

  (realgud-backtrack-set-debugger "jdb")
  (if jdb-backtrack-mode
      (progn
	(realgud-backtrack-mode 't)
	(run-mode-hooks (intern (jdb-backtrack-mode-hook))))
    (progn
      (realgud-backtrack-mode nil)
      ))
)

(defun jdb-backtrack-mode-hook()
  (if jdb-backtrack-mode
      (progn
	(use-local-map jdb-backtrack-mode-map)
	(message "using jdb mode map")
	)
    (message "jdb backtrack-mode-hook disable called"))
)

(provide-me "realgud:jdb-")
