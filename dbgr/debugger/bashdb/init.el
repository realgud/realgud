;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Bash shell debugger: bashdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
		       "dbgr-")
(require-relative-list '("../../lang/posix-shell") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-bashdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a bashdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

;; Regular expression that describes a bashdb command prompt
;; For example: 
;;   bashdb<10>
;;   bashdb<(5)> 
;;   bashdb<<1>>
(setf (gethash "prompt" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^bashdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoints(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `../bashdb/test/example/subshell.sh' at line 6
;;   ##1 source("../bashdb/shell.sh") called from file `/bin/bashdb' at line 140
;;   ##2 main() called from file `/bin/bashdb' at line 0
(setf (gethash "debugger-backtrace" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat dbgr-shell-frame-start-regexp
			dbgr-shell-frame-num-regexp "[ ]?"
			"\\(.*\\)"
			dbgr-shell-frame-file-regexp
			"\\(?:" dbgr-shell-frame-line-regexp "\\)?"
			)
       :num 2
       :file-group 4
       :line-group 5)
      )

;; Regular expression that for a termination message.
(setf (gethash "termination" dbgr-bashdb-pat-hash)
       "^bashdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" dbgr-bashdb-pat-hash)
      '(
	;; The frame number and first type name, if present.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;      --^-
	("^\\(->\\|##\\)\\([0-9]+\\) "
	 (2 dbgr-backtrace-number-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;          ---------^^^^^^^^^^^^^^^^^^^^-
	("[ \t]+\\(in\\|from\\) file `\\(.+\\)'"
	 (2 dbgr-file-name-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;                                         --------^^
	;; Line number.
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 dbgr-line-number-face))
	))

(setf (gethash "bashdb" dbgr-pat-hash) dbgr-bashdb-pat-hash)

(defvar dbgr-bashdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the bashdb command to use, like 'quit!'")

(setf (gethash "quit" dbgr-bashdb-command-hash) "quit!")
(setf (gethash "bashdb" dbgr-command-hash dbgr-bashdb-command-hash))

(provide-me "dbgr-bashdb-")

