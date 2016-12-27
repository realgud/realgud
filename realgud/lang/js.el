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

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(defconst realgud:js-term-escape "[[0-9]+[GKJ]"
  "Escape sequence regular expression pattern trepanjs often puts
  in around prompts")

(declare-function realgud-goto-line-for-pt 'realgud-track)

;;  Regular expression that describes a Perl Carp backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
(defconst realgud:js-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp (format "^\\(?:[\t ]+at \\)?\\([^:]+\\) (\\(.*\\):%s:%s)"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :file-group 2
   :line-group 3
   :char-offset-group 4)
  "A realgud-loc-pat struct that describes a V8 backtrace location")

(provide-me "realgud-lang-")
