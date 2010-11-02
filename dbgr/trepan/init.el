;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/init") 
		       "dbgr-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-trepan-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a trepan location generally shown
;; before a command prompt.
(setf (gethash "loc" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a trepan command prompt
(setf (gethash "prompt" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(+trepan\\(@[0-9]+\\|@main\\)?)+: "
       ))

;;  Regular expression that describes a Ruby backtrace line.
(setf (gethash "backtrace" dbgr-trepan-pat-hash) dbgr-ruby-backtrace-loc-pat)

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+in file \\(.+\\),\n"
       :bp-num 1
       :file-group 3
       :line-group 2))

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :bp-num 1))

(setf (gethash "control-frame" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^c:\\([0-9]+\\) p:\\([0-9]+\\) s:\\([0-9]+\\) b:\\([0-9]+\\) l:\\([0-9a-f]+\\) d:\\([0-9a-f]+\\) \\([A-Z]+\\) \\(.+\\):\\([0-9]+\\)"
       :file-group 8
       :line-group 9))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))

(setf (gethash "font-lock-keywords" dbgr-trepan-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(-->\\)? *#\\([0-9]+\\) \\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?"
	 (2 dbgr-backtrace-number-face)
	 (3 font-lock-keyword-face)         ; e.g. METHOD, TOP
	 (4 font-lock-constant-face)        ; e.g. Object
	 (5 font-lock-function-name-face nil t))   ; t means optional
	;; Instruction sequence
	("<\\(.+\\)>"
	 (1 font-lock-variable-name-face))
	;; "::Type", which occurs in class name of function and in parameter list.
	;; Parameter sequence
	("(\\(.+\\))"
	 (1 font-lock-variable-name-face))
	;; "::Type", which occurs in class name of function and in parameter list.
	("::\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face))
	;; File name.
	("[ \t]+in file \\([^ ]+*\\)"
	 (1 dbgr-file-name-face))
	;; Line number.
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 dbgr-line-number-face))
	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

(setf (gethash "trepan" dbgr-pat-hash) dbgr-trepan-pat-hash)

(provide-me "dbgr-trepan-")
