;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org> Regular
;;; expressions for Korn shell debugger: kshdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
		       "dbgr-")
(require-relative-list '("../../lang/posix-shell") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-kshdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a kshdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" dbgr-kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

;; For example: 
;;   kshdb<10>
;;   kshdb<(5)> 
;;   kshdb<<1>>
(setf (gethash "prompt" dbgr-kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^kshdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" dbgr-kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoints(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/kshdb' at line 129
(setf (gethash "debugger-backtrace" dbgr-kshdb-pat-hash)
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
(setf (gethash "termination" dbgr-kshdb-pat-hash)
       "^kshdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" dbgr-kshdb-pat-hash)
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

(setf (gethash "kshdb" dbgr-pat-hash) dbgr-kshdb-pat-hash)

(defvar dbgr-kshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepan command to use, like 'quit!'")

;; (setf (gethash "quit" dbgr-kshdb-command-hash) "quit!")
(setf (gethash "kshdb" dbgr-command-hash dbgr-kshdb-command-hash))

(setf (gethash "kshdb" dbgr-pat-hash) dbgr-kshdb-pat-hash)

(provide-me "dbgr-kshdb-")
