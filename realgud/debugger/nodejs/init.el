;; Copyright (C) 2015 Free Software Foundation, Inc

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

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud:nodejs-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:nodejs-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(defconst realgud:nodejs-term-escape "[[0-9]+[GKJ]"
  "Escape sequence regular expression pattern nodejs often puts in around prompts")

(defconst realgud:nodejs-frame-start-regexp  "\\(?:^\\|\n\\)\\(?:#\\)")
(defconst realgud:nodejs-frame-num-regexp    "\\([0-9]+\\)")
(defconst realgud:nodejs-frame-module-regexp "[^ \t\n]+")
(defconst realgud:nodejs-frame-file-regexp   "[^ \t\n]+")
(defconst realgud:nodejs-frame-line-regexp   realgud:nodejs-frame-num-regexp)
(defconst realgud:nodejs-frame-column-regexp realgud:nodejs-frame-num-regexp)

;; Regular expression that describes a nodejs location generally shown
;; before a command prompt.
;; For example:
;;   break in /home/indutny/Code/git/indutny/myscript.js:1
(setf (gethash "loc" realgud:nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format
		"\\(?:%s\\)*break in \\([^:]+\\):\\([0-9]*\\)"
		realgud:nodejs-term-escape)
       :file-group 1
       :line-group 2))

;; Regular expression that describes a nodejs command prompt
;; For example:
;;   debug>
(setf (gethash "prompt" realgud:nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^\\(?:%s\\)*debug> " realgud:nodejs-term-escape)
       ))

;;  Regular expression that describes a "breakpoint set" line
;; * 4 var count = 0;
(setf (gethash "brkpt-set" realgud:nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^[*] \\([0-9]+\\) "
       :line-group 1))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoint(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;; #0 module.js:380:17
;; #1 dbgtest.js:3:9
;; #2 Module._compile module.js:456:26
;; #3 Module._extensions..js module.js:474:10
;; #4 Module.load module.js:356:32
;; #5 Module._load module.js:312:12
;; #6 Module.runMain module.js:497:10
; ;#7 timers.js:110:15
(setf (gethash "debugger-backtrace" realgud:nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:nodejs-frame-start-regexp
			realgud:nodejs-frame-num-regexp " "
			"\\(?:" realgud:nodejs-frame-module-regexp " \\)?"
			"\\(" realgud:nodejs-frame-file-regexp "\\)"
			":"
			realgud:nodejs-frame-line-regexp
			":"
			realgud:nodejs-frame-column-regexp
			)
       :num 1
       :file-group 2
       :line-group 3
       :char-offset-group 4))

;; ;; Regular expression that for a termination message.
;; (setf (gethash "termination" realgud:nodejs-pat-hash)
;;        "^nodejs: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:nodejs-pat-hash)
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

(setf (gethash "nodejs" realgud-pat-hash) realgud:nodejs-pat-hash)

(defvar realgud:nodejs-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'finish' and the value is
  the nodejs command to use, like 'out'")

(setf (gethash "nodejs" realgud-command-hash realgud:nodejs-command-hash))

(setf (gethash "backtrace"  realgud:nodejs-command-hash) "T")
(setf (gethash "continue"   realgud:nodejs-command-hash) "cont")
(setf (gethash "quit"       realgud:nodejs-command-hash) "quit")
(setf (gethash "finish"     realgud:nodejs-command-hash) "out")
(setf (gethash "shell"      realgud:nodejs-command-hash)  "repl")
(setf (gethash "step"       realgud:nodejs-command-hash) "step")
(setf (gethash "next"       realgud:nodejs-command-hash) "next")

(provide-me "realgud:nodejs-")
