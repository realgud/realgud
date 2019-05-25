;; Copyright (C) 2017, 2019 Free Software Foundation, Inc

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

;;; Common Java constants and regular expressions.
(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(declare-function realgud-goto-line-for-pt 'realgud-track)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defun realgud-java-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !m") 'realgud:goto-maven-errmsg-line)
  )


(defconst realgud-maven-error-loc-pat
  (make-realgud-loc-pat
   :regexp "^\\[\\(?:ERROR\\|WARNING\\)\\] \\(.*\\):\\[\\([0-9][0-9]*\\),\\([0-9][0-9]*\\)\\]"
   :file-group 1
   :line-group 2
   :char-offset-group 3)
  "A realgud-loc-pat struct that describes a maven error or warning line"
  )

(defun realgud:goto-maven-errmsg-line (pt)
  "Display the location mentioned by the maven error at PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "maven-error"))

(provide-me "realgud-lang-")
