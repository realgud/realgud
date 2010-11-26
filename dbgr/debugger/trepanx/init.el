;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "dbgr-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-trepanx-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a trepanx location generally shown
;; before a command prompt.
(setf (gethash "loc" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a trepanx command prompt
(setf (gethash "prompt" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(+trepanx\\(@[0-9]+\\|@main\\)?)+: "
       ))

;;  Regular expression that describes a Rubinius backtrace line.
(setf (gethash "backtrace" dbgr-trepanx-pat-hash) 
      dbgr-rubinius-backtrace-loc-pat)

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+in file \\(.+\\),\n"
       :num 1
       :file-group 3
       :line-group 2))

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

(setf (gethash "control-frame" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^c:\\([0-9]+\\) p:\\([0-9]+\\) s:\\([0-9]+\\) b:\\([0-9]+\\) l:\\([0-9a-f]+\\) d:\\([0-9a-f]+\\) \\([A-Z]+\\) \\(.+\\):\\([0-9]+\\)"
       :file-group 8
       :line-group 9))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" dbgr-trepanx-pat-hash) dbgr-ruby-dollar-bang)

(setf (gethash "trepanx" dbgr-pat-hash) dbgr-trepanx-pat-hash)

(setf (gethash "font-lock-keywords" dbgr-trepanx-pat-hash)
      '(
	;; File name and line number. E.g. at /test/gcd.rb:6
        ;;                                 ---^^^^^^^^^^^^-^
	(" at \\(.*\\):\\([0-9]+\\)$"
	 (1 dbgr-file-name-face)
	 (2 dbgr-line-number-face))

	;; The frame number and first type name, if present.
	("^ +\\([0-9]+\\) *\\([a-zA-Z_][a-zA-Z0-9_]*\\).*\\(\\([#.]\\)?\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?\\)?"
	 (1 dbgr-backtrace-number-face)
	 (2 font-lock-constant-face)        ; e.g. Object
	 (5 font-lock-function-name-face nil t))   ; t means optional

	;; The frame number and first type name, if present.
	("^ +\\([0-9]+\\) *\\([a-zA-Z_][a-zA-Z0-9_]*\\).*\\(\\([#.]\\)?\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?\\)?"
	 (1 dbgr-backtrace-number-face)
	 (2 font-lock-constant-face)        ; e.g. Object
	 (5 font-lock-function-name-face nil t))   ; t means optional
	;; Parameter sequence
	("(\\(.+\\))"
	 (1 font-lock-variable-name-face))
	;; "::Type", which occurs in class name of function and in parameter list.
	("::\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face))

	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

(provide-me "dbgr-trepanx-")
