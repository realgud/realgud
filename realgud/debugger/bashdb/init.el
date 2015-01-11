;;; Copyright (C) 2010-2011, 2015 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Bash shell debugger: bashdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud:bashdb-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:bashdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a bashdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" realgud:bashdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(?:^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\)):\\(?:\n[0-9]+:	\\(.+\\)\\)?"
       :file-group 1
       :line-group 2
       :text-group 3))

;; Regular expression that describes a bashdb command prompt
;; For example:
;;   bashdb<10>
;;   bashdb<(5)>
;;   bashdb<<1>>
(setf (gethash "prompt" realgud:bashdb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^bashdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:bashdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:bashdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoint(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `../bashdb/test/example/subshell.sh' at line 6
;;   ##1 source("../bashdb/shell.sh") called from file `/bin/bashdb' at line 140
;;   ##2 main() called from file `/bin/bashdb' at line 0
(setf (gethash "debugger-backtrace" realgud:bashdb-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud-shell-frame-start-regexp
			realgud-shell-frame-num-regexp "[ ]?"
			"\\(.*\\)"
			realgud-shell-frame-file-regexp
			"\\(?:" realgud-shell-frame-line-regexp "\\)?"
			)
       :num 2
       :file-group 4
       :line-group 5)
      )

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud:bashdb-pat-hash)
       "^bashdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:bashdb-pat-hash)
      '(
	;; The frame number and first type name, if present.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;      --^-
	("^\\(->\\|##\\)\\([0-9]+\\) "
	 (2 realgud-backtrace-number-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;          ---------^^^^^^^^^^^^^^^^^^^^-
	("[ \t]+\\(in\\|from\\) file `\\(.+\\)'"
	 (2 realgud-file-name-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;                                         --------^^
	;; Line number.
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 realgud-line-number-face))
	))

(setf (gethash "bashdb" realgud-pat-hash) realgud:bashdb-pat-hash)

(defvar realgud:bashdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the bashdb command to use, like 'quit!'")

(setf (gethash "bashdb" realgud-command-hash) realgud:bashdb-command-hash)

(setf (gethash "clear"  realgud:bashdb-command-hash) "clear %l")
(setf (gethash "quit"   realgud:bashdb-command-hash) "quit!")
(setf (gethash "until"  realgud:bashdb-command-hash) "continue %l")

(provide-me "realgud:bashdb-")
