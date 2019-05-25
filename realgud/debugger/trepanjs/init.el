;; Copyright (C) 2015-2016, 2018, 2019 Free Software Foundation, Inc

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

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/js") "realgud-lang-")

(defvar realgud:trepanjs-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc-pat 'realgud-regexp)

;; realgud-loc-pat that describes a trepanjs location generally shown
;; before a command prompt.
;; For example:
;;   break in /home/indutny/Code/git/indutny/myscript.js:1
;;   exception in /usr/lib/nodejs/module.js [module.js]:362
(setf (gethash "loc" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format
		"\\(?:%s\\)*\\(?:break\\|exception\\|call\\) in %s at line %s:%s"
		realgud:js-term-escape realgud:js-file-regexp
		realgud:regexp-captured-num
		realgud:regexp-captured-num)
       :file-group 1
       :line-group 2
       :char-offset-group 3
       ))

(setf (gethash "file-line" realgud:trepanjs-pat-hash) realgud:js-file-line-loc-pat)

;; realgud-loc-pat that describes a trepanjs command prompt
;; For example:
;;   (trepanjs):
(setf (gethash "prompt" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^\\(?:%s\\)*(+trepanjs)+ " realgud:js-term-escape)
       ))

;;  realgud-loc-pat that describes a "breakpoint set" line
;; For example:
;;  Breakpoint 2 set in file /tmp/gcd.js, line 2.
;;  Breakpoint 3 set in file /usr/lib/nodejs/module.js [module.js], line 380.
(setf (gethash "brkpt-set" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s set in file %s, line %s.\n"
		       realgud:regexp-captured-num
		       realgud:js-file-regexp
		       realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3))

;; realgud-loc-pat that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1
(setf (gethash "brkpt-del" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\(\\([0-9]+ *\\)+\\)\n"
       :num 1))

;; realgud-loc-pat that describes a V8 backtrace line.
;; For example:
;;    at repl:1:7
;;    at Interface.controlEval (/src/external-vcs/github/trepanjs/lib/interface.js:352:18)
;;    at REPLServer.b [as eval] (domain.js:183:18)
(setf (gethash "lang-backtrace" realgud:trepanjs-pat-hash)
  realgud:js-backtrace-loc-pat)

;; realgud-loc-pat that describes a debugger "delete" (breakpoint)
;; response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Deleted breakpoint %s.\n"
		       realgud:regexp-captured-num)
       :num 1))


(defconst realgud:trepanjs-frame-start-regexp  "\\(?:^\\|\n\\)\\(?: #\\)")
(defconst realgud:trepanjs-frame-num-regexp    realgud:regexp-captured-num)
(defconst realgud:trepanjs-frame-module-regexp "[^ \t\n]+")

;;  realgud-loc-pat that describes debugger "backtrace" command line.
;;  e.g.
;; ## require called from file /usr/lib/nodejs/module.js [module.js] at line 380:17
;; ## in file /src/external-vcs/github/trepanjs/example/gcd.js [/src/external-vcs/github/trepanjs/example/gcd.js] at line 2:12
(setf (gethash "debugger-backtrace" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:trepanjs-frame-start-regexp " "
			realgud:regexp-captured-num " "
			"\\(?:" realgud:trepanjs-frame-module-regexp "[ \t\n]+called from file "
			realgud:js-file-regexp
			"\\)\\| in file "
			realgud:regexp-captured-num
			"\\)"
			"at line \\(" realgud:regexp-captured-num "\\):"
			realgud:regexp-captured-num
			)
       :num 1
       :file-group 2
       :line-group 3
       :char-offset-group 4
       ))

(defconst realgud:trepanjs-debugger-name "trepanjs" "Name of debugger")

;; Top frame number
(setf (gethash "top-frame-num" realgud:trepanjs-pat-hash) 0)

;; realgud-loc-pat that for a termination message.
(setf (gethash "termination" realgud:trepanjs-pat-hash)
       "^trepanjs: That's all, folks...\n")

(setf (gethash realgud:trepanjs-debugger-name realgud-pat-hash) realgud:trepanjs-pat-hash)

(defvar realgud:trepanjs-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepanjs command to use, like 'quit!'")

(setf (gethash realgud:trepanjs-debugger-name
	       realgud-command-hash) realgud:trepanjs-command-hash)

(setf (gethash "break"      realgud:trepanjs-command-hash)
      "setBreakpoint(%l)")
(setf (gethash "clear"      realgud:trepanjs-command-hash)
      "clearBreakpoint('%X', %l)")
(setf (gethash "delete"      realgud:trepanjs-command-hash)
      "clearBreakpoint('%X', %l)")
;; We need aliases for step and next because the default would
;; do step 1 and trepanjs doesn't handle this. Or when it does,
;; it will probably look like step(1)
(setf (gethash "eval"             realgud:trepanjs-command-hash) "eval(%q)")
(setf (gethash "info-breakpoints" realgud:trepanjs-command-hash) "info('breakpoints')")
(setf (gethash "quit"             realgud:trepanjs-command-hash) "quit()")

;; Unsupported features:
(setf (gethash "kill"             realgud:trepanjs-command-hash) "*not-implemented*")

(provide-me "realgud:trepanjs-")
