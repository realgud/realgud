;;; Copyright (C) 2012 Rocky Bernstein <rocky@gnu.org>
;;; Stock Python debugger pydb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:pydb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

;; Regular expression that describes a pydb location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   > /usr/bin/zonetab2pot.py(15)<module>()
;; or MS Windows:
;;   > c:\\mydirectory\\gcd.py(10)<module>
(setf (gethash "loc" realgud:pydb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" realgud:pydb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^[(]+Pydb[)]+ "
       ))

;;  Regular expression that describes a Python backtrace line.
(setf (gethash "lang-backtrace" realgud:pydb-pat-hash)
      realgud-python-backtrace-loc-pat)

;;  Regular expression that describes a "breakpoint set" line. For example:
;;     Breakpoint 1 at /usr/bin/pydb:7
(setf (gethash "brkpt-set" realgud:pydb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) at[ \t\n]+\\(.+\\):\\([0-9]+\\)\\(\n\\|$\\)"
       :num 1
       :file-group 2
       :line-group 3))

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" realgud:pydb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

(setf (gethash "font-lock-keywords" realgud:pydb-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
	 (2 realgud-backtrace-number-face)
	 (4 font-lock-function-name-face nil t))     ; t means optional.

	;; Parameter sequence, E.g. gcd(a=3, b=5)
	;;                             ^^^^^^^^^
	("(\\(.+\\))"
	 (1 font-lock-variable-name-face))

	;; File name. E.g  file '/test/gcd.py'
	;;                 ------^^^^^^^^^^^^-
	("[ \t]+file '\\([^ ]+*\\)'"
	 (1 realgud-file-name-face))

	;; Line number. E.g. at line 28
        ;;                  ---------^^
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 realgud-line-number-face))

	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (pydb-frames-match-current-line
	;;  (0 pydb-frames-current-frame-face append))
	))

(setf (gethash "pydb" realgud-pat-hash) realgud:pydb-pat-hash)

(defvar realgud:pydb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'shell' and the value is
  the pydb command to use, like 'python'")

(setf (gethash "shell" realgud:pydb-command-hash) "python")
(setf (gethash "pydb" realgud-command-hash) realgud:pydb-command-hash)

(provide-me "realgud:pydb-")
