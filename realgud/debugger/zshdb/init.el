;;; Copyright (C) 2010, 2014 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Z shell debugger: zshdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:zshdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a zshdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" realgud:zshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

;; For example:
;;   zshdb<10>
;;   zshdb<(5)>
;;   zshdb<<1>>
(setf (gethash "prompt" realgud:zshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^zshdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:zshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:zshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoints(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/zshdb' at line 129
(setf (gethash "debugger-backtrace" realgud:zshdb-pat-hash)
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
(setf (gethash "termination" realgud:zshdb-pat-hash)
       "^zshdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:zshdb-pat-hash)
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
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

(defvar realgud:zshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepan command to use, like 'quit!'")

(setf (gethash "quit" realgud:zshdb-command-hash) "quit!")
(setf (gethash "zshdb" realgud-command-hash realgud:zshdb-command-hash))

(setf (gethash "zshdb" realgud-pat-hash) realgud:zshdb-pat-hash)

(provide-me "realgud:zshdb-")
