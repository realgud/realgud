;;; Copyright (C) 2011, 2014-2016, 2019 Free Software Foundation, Inc

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
;;; Regular expressions for nodejs Javascript debugger.
;;; Stock Perl debugger perldb

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "realgud-")
(require-relative-list '("../../lang/perl") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:perldb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
lang-backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

;; Program-location lines look like these:
;;   File::Basename::dirname(/usr/share/perl/5.16.0/File/Basename.pm:284):
;;   File::Basename::dirname(/usr/share/perl/5.16.0/File/Basename.pm:284):	my $path;
;;   main::(/usr/bin/latex2html:102):
;;   main::(/usr/bin/latex2html:102):	@ARGV=2;
;;   main::CODE(0x9407ac8)(l2hconf.pm:6):;;
;;   main::((eval 8)[/tmp/eval.pl:2]:1):
;;
;;   And what are complications MS Windows adds?

;; Hnadle eval form first, e.g.:
;;    main::((eval 8)[/tmp/eval.pl:2]:1):

(defconst realgud:perldb-loc-eval-regexp
  (format "(eval [0-9]+)\\[\\(.+\\):%s\\]"
	  realgud:regexp-captured-num))

;; Hnadle non eval form
;;    main::CODE(0x9407ac8)(l2hconf.pm:6):;;

(defconst realgud:perldb-loc-noeval-regexp
  (format "\\(?:CODE(0x[0-9a-h]+)\\)?(\\(.+\\):%s):\\(?:\t\\(.*\\)\\)?\n"
	  realgud:regexp-captured-num))

;; Note that eval form has to come before non-eval form as the non-eval
;; form encompases the eval form. The two clauses makes it hard
;; to match file and line positions, so we ned to result to the
;; "alt" forms of file and lines as well as the non-alt formes
(defconst realgud:perldb-loc-regexp
  (format "\\(?:%s\\)\\|\\(?:%s\\)"
	  realgud:perldb-loc-eval-regexp realgud:perldb-loc-noeval-regexp))

;; Regular expression that describes a perldb location generally shown
;; before a command prompt. We include matching the source text so we
;; can save that.
(setf (gethash "loc" realgud:perldb-pat-hash)
      (make-realgud-loc-pat
       :regexp realgud:perldb-loc-regexp
       :alt-file-group 1
       :alt-line-group 2
       :file-group 3
       :line-group 4
       :text-group 5))

;; perldb debugger prompt.
;; Examples:
;;   DB<4>
;; [pid=6489->6502]  DB<1>
;;
(setf (gethash "prompt" realgud:perldb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "\\(?:\\[pid=[0-9]+->[0-9]+\\]\\)?  DB<\\([0-9]+\\)> "
       :num 1
       ))

;;  Regular expression that describes a Perl debugger backtrace line.
;; $ = main::top_navigation_panel called from file `./latex2html' line 7400
;; $ = main::BEGIN() called from file `(eval 19)[/usr/bin/latex2html:126]' line 2
(setf (gethash "debugger-backtrace" realgud:perldb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "\s+called from file `\\(.+\\)' line \\([0-9]+\\)"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes location in a Perl errmsg
(setf (gethash "perl-errmsg" realgud:perldb-pat-hash)
      realgud-perl-errmsg-loc-pat)

;;  Regular expression that describes a Perl Carp backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
(setf (gethash "lang-backtrace" realgud:perldb-pat-hash)
      realgud-perl-carp-loc-pat)

(defvar realgud:perldb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the perldb command to use, like 'q'")

(setf (gethash "font-lock-keywords" realgud:perldb-pat-hash)
      '(
	("\s+called from file `\\(.+\\)' line \\([0-9]+\\)"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))
	))


(setf (gethash "perldb"  realgud-pat-hash) realgud:perldb-pat-hash)

(setf (gethash "backtrace"        realgud:perldb-command-hash) "T")
(setf (gethash "break"            realgud:perldb-command-hash) "b %l")
(setf (gethash "clear"            realgud:perldb-command-hash) "B %l")
(setf (gethash "continue"         realgud:perldb-command-hash) "c")
(setf (gethash "eval"             realgud:perldb-command-hash) "x %s")
(setf (gethash "info-breakpoints" realgud:perldb-command-hash) "L")
(setf (gethash "quit"             realgud:perldb-command-hash) "q")
(setf (gethash "restart"          realgud:perldb-command-hash) "R")
(setf (gethash "run"              realgud:perldb-command-hash) "R")
(setf (gethash "step"             realgud:perldb-command-hash) "s")
(setf (gethash "next"             realgud:perldb-command-hash) "n")
(setf (gethash "until"            realgud:perldb-command-hash) "c %l")
(setf (gethash "perldb"           realgud-command-hash) realgud:perldb-command-hash)

;; Unsupported features:
(setf (gethash "frame" realgud:perldb-command-hash) "*not-implemented*")
(setf (gethash "shell" realgud:perldb-command-hash) "*not-implemented*")
(setf (gethash "up"    realgud:perldb-command-hash) "*not-implemented*")
(setf (gethash "down"  realgud:perldb-command-hash) "*not-implemented*")
(setf (gethash "jump"  realgud:perldb-command-hash) "*not-implemented*")
(setf (gethash "kill"  realgud:perldb-command-hash) "*not-implemented*")

(provide-me "realgud:perldb-")
