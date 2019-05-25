;; Copyright (C) 2015-2016, 2018-2019 Free Software Foundation, Inc

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

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(declare-function realgud-goto-line-for-pt 'realgud-track)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defconst realgud:js-term-escape "[[0-9]+[GKJ]"
  "Escape sequence regular expression pattern trepanjs often puts
  in around prompts")

(defconst realgud:js-file-regexp "\\([^ \t\n]+\\)\\(?: \\[.*\\]\\)?")



(declare-function realgud-goto-line-for-pt 'realgud-track)

;;  Regular expression that describes a javascript backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
(defconst realgud:js-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp (format "^\\(?:[<]?[\t ]+at \\)?\\([^:]+\\) (\\(.*\\):%s:%s)"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :file-group 2
   :line-group 3
   :char-offset-group 4)
  "A realgud-loc-pat struct that describes a V8 backtrace location")

;;  Regular expression that describes a javascript backtrace line.
;; For example:
;;   1 breakpoint y   /usr/local/lib/node_modules/trepanjs/bin/trepanjs:1
;;   	breakpoint already hit 1 time
;;   2 breakpoint y   /usr/local/lib/node_modules/trepanjs/bin/trepanjs:5
;    3 breakpoint y   /usr/local/lib/node_modules/trepanjs/bin/trepanjs:6
(defconst realgud:js-debugger-backtrace-pat
  (make-realgud-loc-pat
   :regexp (format "^%s[ \t]+\\(breakpoint\\)[ \t]+\\([yn]\\)[ \t]+.*at \\(.+\\):%s"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :num 1
   :text-group 2  ;; misnamed Is "breakpoint" or "watchpoint"
   :string 3      ;; misnamed. Is "y/n"
   :file-group 4
   :line-group 5)
  "A realgud-loc-pat struct that describes a V8 breakpoint location")

(defconst realgud:js-file-line-loc-pat
  (make-realgud-loc-pat
   :regexp (format "^\\([^:]+\\):%s" realgud:regexp-captured-num)
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a V8 file/line location")

(defconst realgud:js-file-line-loc-pat
  (make-realgud-loc-pat
   :regexp (format "^\\([^:]+\\):%s" realgud:regexp-captured-num)
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a V8 file/line location")

;; FIXME: there is probably a less redundant way to do the following
;; FNS.
(defun realgud:js-goto-file-line (pt)
  "Display the location mentioned by the js file/line warning or error."
  (interactive "d")
  (realgud-goto-line-for-pt pt "file-line"))

(defun realgud:js-goto-syntax-error-line (pt)
  "Display the location mentioned in a Syntax error line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "syntax-error"))


(provide-me "realgud-lang-")
