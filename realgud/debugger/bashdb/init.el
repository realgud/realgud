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

;; Regular expressions for Bash shell debugger: bashdb

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
(setf (gethash "loc" realgud:bashdb-pat-hash) realgud:POSIX-debugger-loc-pat)

;; Regular expression that describes a bashdb command prompt
;; For example:
;;   bashdb<10>
;;   bashdb<(5)>
;;   bashdb<<1>>
(setf (gethash "prompt" realgud:bashdb-pat-hash)
      (make-realgud-loc-pat
       :regexp  (format  "^bashdb[<]+[(]*%s[)]*[>]+ "
			 realgud:regexp-captured-num)
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-brkpt-set-pat)

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-brkpt-del-pat)

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `../bashdb/test/example/subshell.sh' at line 6
;;   ##1 source("../bashdb/shell.sh") called from file `/bin/bashdb' at line 140
;;   ##2 main() called from file `/bin/bashdb' at line 0
(setf (gethash "debugger-backtrace" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-backtrace-pat)

;; Regular expression for a termination message.
(setf (gethash "termination" realgud:bashdb-pat-hash)
       "^bashdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-font-lock-keywords)

(setf (gethash "bashdb" realgud-pat-hash) realgud:bashdb-pat-hash)

(defvar realgud:bashdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the bashdb command to use, like 'quit!'")

(setf (gethash "bashdb" realgud-command-hash) realgud:bashdb-command-hash)

(setf (gethash "clear"  realgud:bashdb-command-hash) "clear %l")
(setf (gethash "quit"   realgud:bashdb-command-hash) "quit!")
(setf (gethash "until"  realgud:bashdb-command-hash) "continue %l")

(provide-me "realgud:bashdb-")
