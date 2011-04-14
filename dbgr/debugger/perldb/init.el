;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Stock Perl debugger perldb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-perldb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
lang-backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a perldb location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   main::(/usr/bin/latex2html:102):
;;   main::CODE(0x9407ac8)(l2hconf.pm:6):
;; or MS Windows:
;;   ???
(setf (gethash "loc" dbgr-perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:CODE(0x[0-9a-h]+)\\)?(\\(.+\\):\\(\[0-9]+\\)):"
       :file-group 1
       :line-group 2))

;; perldb debugger prompt.
;; Examples:
;;   DB<4> 
;; [pid=6489->6502]  DB<1> 
;; 
(setf (gethash "prompt" dbgr-perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "\\(?:\\[pid=[0-9]+->[0-9]+\\]\\)?  DB<\\([0-9]+\\)> "
       :num 1
       ))

;;  Regular expression that describes a Perl debugger backtrace line.
;; $ = main::top_navigation_panel called from file `./latex2html' line 7400
;; $ = main::BEGIN() called from file `(eval 19)[/usr/bin/latex2html:126]' line 2
(setf (gethash "debugger-backtrace" dbgr-perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "\s+called from file `\\(.+\\)' line \\([0-9]+\\)"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Perl Carp backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
2
(setf (gethash "lang-backtrace" dbgr-perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   (concat 
		  "\\(?:^\\|
\\)"
		  "\\(?:[ \t]+\\(?:\\|.* called \\)at \\(.*\\) line \\([0-9]+\\)\\)")
       :file-group 1
       :line-group 2))

(defvar dbgr-perldb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the perldb command to use, like 'q'")

(setf (gethash "font-lock-keywords" dbgr-perldb-pat-hash)
      '(
	("\s+called from file `\\(.+\\)' line \\([0-9]+\\)"
	 (1 dbgr-file-name-face)
	 (2 dbgr-line-number-face))
	))


(setf (gethash "perldb" dbgr-pat-hash) dbgr-perldb-pat-hash)

(setf (gethash "backtrace" dbgr-perldb-command-hash) "T")
(setf (gethash "break"     dbgr-perldb-command-hash) "b %l")
(setf (gethash "continue"  dbgr-perldb-command-hash) "c")
(setf (gethash "eval"      dbgr-perldb-command-hash) "x %s")
(setf (gethash "quit"      dbgr-perldb-command-hash) "q")
(setf (gethash "restart"   dbgr-perldb-command-hash) "R")
(setf (gethash "run"       dbgr-perldb-command-hash) "R")
(setf (gethash "step"      dbgr-perldb-command-hash) "s")
(setf (gethash "next"      dbgr-perldb-command-hash) "n")
(setf (gethash "perldb" dbgr-command-hash) dbgr-perldb-command-hash)

(provide-me "dbgr-perldb-")
