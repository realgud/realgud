;; Copyright (C) 2010-2011, 2016-2017, 2019 Free Software Foundation, Inc

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
(declare-function make-realgud-loc-pat 'realgud-regexp)

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

;;  realgud-loc-pat that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-brkpt-set-pat)

;; realgud-loc-pat that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-brkpt-del-pat)

;; realgud-loc-pat that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 disabled.
(setf (gethash "brkpt-disable" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-brkpt-disable-pat)

;; realgud-loc-pat that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 enabled.
(setf (gethash "brkpt-enable" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-brkpt-enable-pat)

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/kshdb' at line 129
(setf (gethash "debugger-backtrace" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-backtrace-pat)

;; FIXME breakpoints aren't locations. It should be a different structure
;; realgud-loc that describes a zshdb "info breakpoints" line.
(setf (gethash "debugger-breakpoint" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-backtrace-pat)

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud:kshdb-pat-hash)
       "^kshdb: That's all, folks...\n")

;; FIXME breakpoints aren't locations. It should be a different structure
;; realgud-loc that describes a zshdb "info breakpoints" line.
(setf (gethash "debugger-breakpoint" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-breakpoint-pat)

(setf (gethash "font-lock-keywords" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-font-lock-keywords)

(setf (gethash "font-lock-breakpoint-keywords" realgud:kshdb-pat-hash)
      realgud:POSIX-debugger-font-lock-breakpoint-keywords)

(setf (gethash "kshdb" realgud-pat-hash) realgud:kshdb-pat-hash)

(defvar realgud:kshdb-command-hash (make-hash-table :test 'equal)
  "hash key is command name like 'quit' and the value is
  the trepan command to use, like 'quit!'")

(setf (gethash "kshdb" realgud-command-hash) realgud:kshdb-command-hash)

;; (setf (gethash "quit" realgud:kshdb-command-hash) "quit!")


(setf (gethash "clear"            realgud:kshdb-command-hash) "clear %l")
(setf (gethash "eval"             realgud:kshdb-command-hash) "eval %s")
(setf (gethash "info-breakpoints" realgud:kshdb-command-hash) "info breakpoints")
(setf (gethash "quit"             realgud:kshdb-command-hash) "quit")
(setf (gethash "until"            realgud:kshdb-command-hash) "continue %l")

;; Unsupported features:
(setf (gethash "jump"   realgud:kshdb-command-hash) "*not-implemented*")
(setf (gethash "finish" realgud:kshdb-command-hash) "*not-implemented*")
(setf (gethash "jump"   realgud:kshdb-command-hash) "*not-implemented*")

(provide-me "realgud:kshdb-")
