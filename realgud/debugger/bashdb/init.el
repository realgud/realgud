;; Copyright (C) 2015-2017, 2019 Free Software Foundation, Inc

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

(eval-when-compile (require 'cl-lib))   ;For setf.
(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud:bashdb-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:bashdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a bashdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" realgud:bashdb-pat-hash) realgud:POSIX-debugger-loc-pat)

;; Top frame number
(setf (gethash "top-frame-num" realgud:bashdb-pat-hash) 0)

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

;;  realgud-loc-pat that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-brkpt-set-pat)

;; realgud-loc-pat that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-brkpt-del-pat)

;; realgud-loc-pat that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 disabled.
(setf (gethash "brkpt-disable" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-brkpt-disable-pat)

;; realgud-loc-pat that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 enabled.
(setf (gethash "brkpt-enable" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-brkpt-enable-pat)

;; realgud-loc-pat that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `../bashdb/test/example/subshell.sh' at line 6
;;   ##1 source("../bashdb/shell.sh") called from file `/bin/bashdb' at line 140
;;   ##2 main() called from file `/bin/bashdb' at line 0
(setf (gethash "debugger-backtrace" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-backtrace-pat)

;; FIXME breakpoints aren't locations. It should be a different structure
;; realgud-loc that describes a zshdb "info breakpoints" line.
(setf (gethash "debugger-breakpoint" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-breakpoint-pat)

;; realgud-loc-pat for a termination message.
(setf (gethash "termination" realgud:bashdb-pat-hash)
       "^bashdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-font-lock-keywords)

(setf (gethash "font-lock-breakpoint-keywords" realgud:bashdb-pat-hash)
      realgud:POSIX-debugger-font-lock-breakpoint-keywords)

(setf (gethash "bashdb" realgud-pat-hash) realgud:bashdb-pat-hash)

(defvar realgud:bashdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
the bashdb command to use, like 'quit!'")

(setf (gethash "bashdb" realgud-command-hash) realgud:bashdb-command-hash)

(setf (gethash "clear"            realgud:bashdb-command-hash) "clear %l")
(setf (gethash "eval"             realgud:bashdb-command-hash) "eval %s")
(setf (gethash "info-breakpoints" realgud:bashdb-command-hash) "info breakpoints")
(setf (gethash "quit"             realgud:bashdb-command-hash) "quit")
(setf (gethash "until"            realgud:bashdb-command-hash) "continue %l")

;; Unsupported features:
(setf (gethash "break-fn"   realgud:bashdb-command-hash) "*not-implemented*")
(setf (gethash "finish"     realgud:bashdb-command-hash) "*not-implemented*")
(setf (gethash "jump"       realgud:bashdb-command-hash) "*not-implemented*")

(provide-me "realgud:bashdb-")
