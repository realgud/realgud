;;; Copyright (C) 2011, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; Stock Perl debugger perldb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "realgud-")
(require-relative-list '("../../lang/perl") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:perldb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
lang-backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

;; Regular expression that describes a perldb location generally shown
;; before a command prompt. We include matching the source text so we
;; can save that.
;;
;; Program-location lines look like this:
;;  File::Basename::dirname(/usr/share/perl/5.16.0/File/Basename.pm:284):
;;  284:	    my $path = shift;
;;
;;   main::(/usr/bin/latex2html:102):
;; or MS Windows:
;;   ???
(setf (gethash "loc" realgud:perldb-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(?:CODE(0x[0-9a-h]+)\\)?(\\(.+\\):\\(\[0-9]+\\)):\\(?:\n[0-9]+:\t\\(.*?\\)\n\\)?"
       :file-group 1
       :line-group 2
       :text-group 3))

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
(setf (gethash "perl5db" realgud-pat-hash) realgud:perldb-pat-hash)

(setf (gethash "backtrace" realgud:perldb-command-hash) "T")
(setf (gethash "break"     realgud:perldb-command-hash) "b %l")
(setf (gethash "clear"     realgud:perldb-command-hash) "B %l")
(setf (gethash "continue"  realgud:perldb-command-hash) "c")
(setf (gethash "eval"      realgud:perldb-command-hash) "x %s")
(setf (gethash "quit"      realgud:perldb-command-hash) "q")
(setf (gethash "restart"   realgud:perldb-command-hash) "R")
(setf (gethash "run"       realgud:perldb-command-hash) "R")
(setf (gethash "step"      realgud:perldb-command-hash) "s")
(setf (gethash "next"      realgud:perldb-command-hash) "n")
(setf (gethash "until"     realgud:perldb-command-hash) "c %l")
(setf (gethash "perldb" realgud-command-hash) realgud:perldb-command-hash)

(provide-me "realgud:perldb-")
