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

(defvar realgud:trepanjs-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc-pat (realgud-loc))

(defconst realgud:trepanjs-term-escape "[[0-9]+[GKJ]"
  "Escape sequence regular expression pattern trepanjs often puts
  in around prompts")

(defconst realgud:nodejs-frame-start-regexp  "\\(?:^\\|\n\\)\\(?:#\\)")
(defconst realgud:nodejs-frame-num-regexp    "\\([0-9]+\\)")
(defconst realgud:nodejs-frame-module-regexp "[^ \t\n]+")
(defconst realgud:nodejs-frame-file-regexp   "[^ \t\n]+")
(defconst realgud:nodejs-frame-line-regexp   realgud:nodejs-frame-num-regexp)
(defconst realgud:nodejs-frame-column-regexp realgud:nodejs-frame-num-regexp)

;; Regular expression that describes a trepanjs location generally shown
;; before a command prompt.
;; For example:
;;   break in /home/indutny/Code/git/indutny/myscript.js:1
(setf (gethash "loc" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format
		"\\(?:%s\\)*\\(?:break\\|exception\\) in \\([^:]+\\):\\([0-9]*\\)"
		realgud:trepanjs-term-escape)
       :file-group 1
       :line-group 2))

;; Regular expression that describes a trepanjs command prompt
;; For example:
;;   (trepanjs):
(setf (gethash "prompt" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^(+trepanjs)+ "
       ))

;;  Regular expression that describes a "breakpoint set" line
;; * 4 var count = 0;
(setf (gethash "brkpt-set" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a V8 backtrace line.
;; For example:
;;    at repl:1:7
;;    at Interface.controlEval (/src/external-vcs/github/trepanjs/lib/interface.js:352:18)
;;    at REPLServer.b [as eval] (domain.js:183:18)
(setf (gethash "lang-backtrace" realgud:trepanjs-pat-hash)
  (make-realgud-loc-pat
   :regexp "^\\(?:[\t ]+at \\)?\\([^:]+\\) (.*:\\([0-9]+\\):\\([0-9]+\\))"
   :file-group 2
   :line-group 3
   :char-offset-group 4))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:trepanjs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\).\n"
       :num 1))

(defconst realgud:trepanjs-debugger-name "trepanjs" "Name of debugger")

;; Top frame number
(setf (gethash "top-frame-num" realgud:trepanjs-pat-hash) 0)

;; Regular expression that for a termination message.
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
;; We need aliases for step and next because the default would
;; do step 1 and trepanjs doesn't handle this. Or when it does,
;; it will probably look like step(1)
(setf (gethash "step"       realgud:trepanjs-command-hash) "step")
(setf (gethash "next"       realgud:trepanjs-command-hash) "next")

(provide-me "realgud:trepanjs-")
