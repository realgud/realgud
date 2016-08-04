;; Copyright (C) 2010-2011, 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;Regular expressions for Korn shell debugger: kshdb

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:kshdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Top frame number
(setf (gethash "top-frame-num" realgud:kshdb-pat-hash) 0)

;; Regular expression that describes a kshdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" realgud:kshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

;; For example:
;;   kshdb<10>
;;   kshdb<(5)>
;;   kshdb<<1>>
(setf (gethash "prompt" realgud:kshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^kshdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:kshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:kshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoints(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/kshdb' at line 129
(setf (gethash "debugger-backtrace" realgud:kshdb-pat-hash)
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
(setf (gethash "termination" realgud:kshdb-pat-hash)
       "^kshdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:kshdb-pat-hash)
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

(setf (gethash "kshdb" realgud-pat-hash) realgud:kshdb-pat-hash)

(defvar realgud:kshdb-command-hash (make-hash-table :test 'equal)
  "hash key is command name like 'quit' and the value is
  the trepan command to use, like 'quit!'")

;; (setf (gethash "quit" realgud:kshdb-command-hash) "quit!")

(setf (gethash "kshdb" realgud-pat-hash) realgud:kshdb-pat-hash)
(setf (gethash "clear" realgud:kshdb-command-hash) "clear %l")

(provide-me "realgud:kshdb-")
