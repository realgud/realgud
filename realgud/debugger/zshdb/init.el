;;; Copyright (C) 2010, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Z shell debugger: zshdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud:zshdb-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:zshdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a zshdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(setf (gethash "loc" realgud:zshdb-pat-hash) realgud:POSIX-debugger-loc-pat)

;; Regular expression that describes a zshdb command prompt
;; For example:
;;   zshdb<10>
;;   zshdb<(5)>
;;   zshdb<<1>>
(setf (gethash "prompt" realgud:zshdb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^zshdb[<]+[(]*\\([0-9]+\\)[)]*[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line.
(setf (gethash "brkpt-set" realgud:zshdb-pat-hash)
      realgud:POSIX-debugger-brkpt-set-pat)

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:zshdb-pat-hash)
      realgud:POSIX-debugger-brkpt-del-pat)

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/zshdb' at line 129
(setf (gethash "debugger-backtrace" realgud:zshdb-pat-hash)
      realgud:POSIX-debugger-backtrace-pat)

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud:zshdb-pat-hash)
       "^zshdb: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:zshdb-pat-hash)
      realgud:POSIX-debugger-font-lock-keywords)

(setf (gethash "zshdb" realgud-pat-hash) realgud:zshdb-pat-hash)

(defvar realgud:zshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the zshdb command to use, like 'quit!'")

(setf (gethash "zshdb"  realgud-command-hash) realgud:zshdb-command-hash)

(setf (gethash "clear"  realgud:zshdb-command-hash) "clear %l")
(setf (gethash "quit"   realgud:zshdb-command-hash) "quit")
(setf (gethash "until"  realgud:zshdb-command-hash) "continue %l")

(provide-me "realgud:zshdb-")
