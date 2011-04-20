;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
;;; gdb debugger

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-gdb-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  the values of a hash entry is a
dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

(defconst dbgr-gdb-frame-file-regexp
 "\\(.+\\):\\([0-9]+\\)")

;; regular expression that describes a gdb location generally shown
;; before a command prompt. NOTE: we assume annotate 1!
(setf (gethash "loc" dbgr-gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp (format "^%s:\\([0-9]+\\):beg:0x\\([0-9a-f]+\\)"
		       dbgr-gdb-frame-file-regexp)
       :file-group 1
       :line-group 2
       :char-offset-group 3))

(setf (gethash "prompt" dbgr-gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^(gdb) "
       ))

;;  regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) at 0x\\([0-9a-f]*\\): file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 3
       :line-group 4))

(defconst dbgr-gdb-frame-start-regexp
  "\\(?:^\\|\n\\)")

(defconst dbgr-gdb-frame-num-regexp
  "#\\([0-9]+\\) ")

;; Regular expression that describes a gdb "backtrace" command line.
;; For example:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; #1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 "/tmp/remake/remake") at strdup.c:42
;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
;;    at main.c:952
;; #46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0, 
;;    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410

(setf (gethash "debugger-backtrace" dbgr-gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat dbgr-gdb-frame-start-regexp 
			dbgr-gdb-frame-num-regexp
			"\\(?:.\\|\\(?:[\n] \\)\\)+[ ]+at "
			dbgr-gdb-frame-file-regexp
			)
       :num 1
       :file-group 2
       :line-group 3)
      )

(setf (gethash "font-lock-keywords" dbgr-gdb-pat-hash)
      '(
	;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
	;;    at main.c:952
	("[ \n]+at \\(.*\\):\\([0-9]+\\)"
	 (1 dbgr-file-name-face)
	 (2 dbgr-line-number-face))

	;; The frame number and first type name, if present.
	;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;;      ---^
	( "#\\(?:^\\|\n\\)\\([0-9]+\\)  "
	 (1 dbgr-backtrace-number-face))
	))

(setf (gethash "gdb" dbgr-pat-hash) dbgr-gdb-pat-hash)

(defvar dbgr-gdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'continue' and the value is 
  the gdb command to use, like 'continue'")

(setf (gethash "break"    dbgr-gdb-command-hash) "break %l")
(setf (gethash "continue" dbgr-gdb-command-hash) "continue")
(setf (gethash "quit"     dbgr-gdb-command-hash) "quit")
(setf (gethash "run"      dbgr-gdb-command-hash) "run")
(setf (gethash "step"     dbgr-gdb-command-hash) "step %p")
(setf (gethash "gdb" dbgr-command-hash) dbgr-gdb-command-hash)

(setf (gethash "gdb" dbgr-pat-hash) dbgr-gdb-pat-hash)

(provide-me "dbgr-gdb-")
