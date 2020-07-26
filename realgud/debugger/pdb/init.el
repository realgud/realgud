;; Copyright (C) 2015-2016, 2018-2019 Free Software Foundation, Inc

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
;; Stock Python debugger pdb

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:pdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

;; -------------------------------------------------------------------
;; User-definable variables
;;

;; Regular expression that describes a pdb location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   > /usr/bin/zonetab2pot.py(15)<module>()
;; or MS Windows:
;;   > c:\\mydirectory\\gcd.py(10)<module>
(setf (gethash "loc" realgud:pdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^> \\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\)(\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;; An initial list of regexps that don't generally have files
;; associated with them and therefore we should not try to find file
;; associations for them.  This list is used to seed a field of the
;; same name in the cmd-info structure inside a command buffer. A user
;; may add additional files to the command-buffer's re-ignore-list.
(setf (gethash "ignore-re-file-list" realgud:pdb-pat-hash)
      (list realgud-python-ignore-file-re))

(setf (gethash "prompt" realgud:pdb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^[(]+Pdb[)]+ "
       ))

;; realgud-loc-pat that describes a Python backtrace line.
(setf (gethash "lang-backtrace" realgud:pdb-pat-hash)
      realgud-python-backtrace-loc-pat)

(setf (gethash "debugger-backtrace" realgud:pdb-pat-hash)
      realgud:python-trepan-backtrace-pat)

;;  realgud-loc-pat that describes a line a Python "info break" line.
;; For example:
;; 1   breakpoint    keep y   at /usr/local/bin/trepan3k:7
(setf (gethash "debugger-breakpoint" realgud:pdb-pat-hash)
  (make-realgud-loc-pat
   :regexp (format "^%s[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)[ \t]+\\(yes\\|no\\)[ \t]+.*at \\(.+\\):%s"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :num 1
   :text-group 2  ;; misnamed Is "breakpoint" or "watchpoint"
   :string 3      ;; misnamed. Is "keep" or "del"
   :file-group 5
   :line-group 6))

;;  realgud-loc-pat that describes location in a pytest error
(setf (gethash "pytest-error" realgud:pdb-pat-hash)
      realgud-pytest-error-loc-pat)

;;  realgud-loc-pat that describes location in a flake8 message
(setf (gethash "flake8-msg" realgud:pdb-pat-hash)
      realgud-flake8-msg-loc-pat)

;;  Regular expression that describes a "breakpoint set" line. For example:
;;     Breakpoint 1 at /usr/bin/pdb:7
(setf (gethash "brkpt-set" realgud:pdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) at[ \t\n]+\\(.+\\):\\([0-9]+\\)\\(\n\\|$\\)"
       :num 1
       :file-group 2
       :line-group 3))

;; realgud-loc-pat that describes a "delete breakpoint" line
;; Python 3 includes a file name and line number; Python 2 doesn't
(setf (gethash "brkpt-del" realgud:pdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)"
       :num 1))

(setf (gethash "font-lock-keywords" realgud:pdb-pat-hash)
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
	;; (pdb-frames-match-current-line
	;;  (0 pdb-frames-current-frame-face append))
	))

(setf (gethash "font-lock-breakpoint-keywords" realgud:pdb-pat-hash)
      realgud:python-debugger-font-lock-breakpoint-keywords)

(defvar realgud:pdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'finish' and the value is
the pdb command to use, like 'return'")

(setf (gethash "pdb" realgud-command-hash) realgud:pdb-command-hash)

;; Mappings between PDB-specific names and GUD names
(setf (gethash "finish"           realgud:pdb-command-hash) "return")
(setf (gethash "kill"             realgud:pdb-command-hash) "quit")
(setf (gethash "backtrace"        realgud:pdb-command-hash) "where")
;; Clear in Python does both the usual “delete” and “clear”
(setf (gethash "delete"           realgud:pdb-command-hash) "clear %p")
(setf (gethash "clear"            realgud:pdb-command-hash) "clear %X:%l")
;; Use ‘!’ instead of ‘p’, since ‘p’ only works for expressions, not statements
(setf (gethash "eval"             realgud:pdb-command-hash) "!%s")
(setf (gethash "info-breakpoints" realgud:pdb-command-hash) "break")

(setf (gethash "info-locals-names-list" realgud:pdb-command-hash) "print('\\n'.join(locals().keys()))")
(setf (gethash "info-value" realgud:pdb-command-hash) "pp %s")
(setf (gethash "info-type" realgud:pdb-command-hash) "type(%s)")

;; Unsupported features:
(setf (gethash "shell" realgud:pdb-command-hash) "*not-implemented*")
(setf (gethash "frame" realgud:pdb-command-hash) "*not-implemented*")

(setf (gethash "pdb" realgud-pat-hash) realgud:pdb-pat-hash)

(provide-me "realgud:pdb-")
