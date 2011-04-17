;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Z shell debugger: zshdb 

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
		       "dbgr-")
(require-relative-list '("../../lang/posix-shell") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-zshdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a zshdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" dbgr-zshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

;; For example: 
;;   zshdb<10>
;;   zshdb<(5)> 
;;   zshdb<<1>>
(setf (gethash "prompt" dbgr-zshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^zshdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-zshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" dbgr-zshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoints(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/zshdb' at line 129
(setf (gethash "debugger-backtrace" dbgr-zshdb-pat-hash)
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
(setf (gethash "termination" dbgr-zshdb-pat-hash)
       "^zshdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" dbgr-zshdb-pat-hash)
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
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

(defvar dbgr-zshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepan command to use, like 'quit!'")

(setf (gethash "quit" dbgr-zshdb-command-hash) "quit!")
(setf (gethash "zshdb" dbgr-command-hash dbgr-zshdb-command-hash))

(setf (gethash "zshdb" dbgr-pat-hash) dbgr-zshdb-pat-hash)

(provide-me "dbgr-zshdb-")
