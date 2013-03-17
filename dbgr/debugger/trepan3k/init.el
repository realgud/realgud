;;; Copyright (C) 2010-2013 Rocky Bernstein <rocky@gnu.org>
;;; trepan3k: Python 3.2 and beyond

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init")
		       "dbgr-")
(require-relative-list '("../../lang/python") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-trepan3k-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a trepan3k location generally shown
;; before a command prompt.
;;
;; For example:
;;   (/usr/bin/zonetab2pot.py:15): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
(setf (gethash "loc" dbgr-trepan3k-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\)\\(?: remapped .*\\)?)"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" dbgr-trepan3k-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^(Trepan3k) "
       ))

;;  Regular expression that describes a Python backtrace line.
(setf (gethash "lang-backtrace" dbgr-trepan3k-pat-hash) 
      dbgr-python-backtrace-loc-pat)

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-trepan3k-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+of file \\(.+\\)\\(\n\\|$\\)"
       :num 1
       :file-group 3
       :line-group 2))

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" dbgr-trepan3k-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

(setf (gethash "font-lock-keywords" dbgr-trepan3k-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
	 (2 dbgr-backtrace-number-face)
	 (4 font-lock-function-name-face nil t))     ; t means optional.

	;; Parameter sequence, E.g. gcd(a=3, b=5)
	;;                             ^^^^^^^^^
	("(\\(.+\\))"
	 (1 font-lock-variable-name-face))

	;; File name. E.g  file '/test/gcd.py'
	;;                 ------^^^^^^^^^^^^-
	("[ \t]+file '\\([^ ]+*\\)'"
	 (1 dbgr-file-name-face))

	;; Line number. E.g. at line 28
        ;;                  ---------^^
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 dbgr-line-number-face))

	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (trepan3k-frames-match-current-line
	;;  (0 trepan3k-frames-current-frame-face append))
	))

(setf (gethash "trepan3k" dbgr-pat-hash) dbgr-trepan3k-pat-hash)

(defvar dbgr-trepan3k-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'shell' and the value is 
  the trepan3k command to use, like 'python'")

(setf (gethash "shell" dbgr-trepan3k-command-hash) "python")
(setf (gethash "trepan3k" dbgr-command-hash) dbgr-trepan3k-command-hash)

(provide-me "dbgr-trepan3k-")
