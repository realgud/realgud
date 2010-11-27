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
;; For example: 
;;  -> (/tmp/fact.rb:1)
;;  -- (kernel/common/scope.rb:134 remapped /tmp/scope.rb:134)
(setf (gethash "loc" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a trepanx command prompt
;; For example: 
;;   (trepanx): 
;;   ((trepanx)):
(setf (gethash "prompt" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(+trepanx\\(@[0-9]+\\|@main\\)?)+: "
       ))

;;  Regular expression that describes a Rubinius backtrace line.
(setf (gethash "backtrace" dbgr-trepanx-pat-hash) 
      dbgr-rubinius-backtrace-loc-pat)

;; Regular expression that describes a "breakpoint set" line
;; For example: 
;; Set breakpoint 1: /tmp/fact.rb:1 (@0)
(setf (gethash "brkpt-set" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\): \\(.+\\):\\([0-9]+\\) (@[0-9]+)\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a "delete breakpoint" line
;; For example:'
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\).\n"
       :num 1))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" dbgr-trepanx-pat-hash) dbgr-ruby-dollar-bang)

(setf (gethash "trepanx" dbgr-pat-hash) dbgr-trepanx-pat-hash)

(defconst dbgr-trepanx-frame-file-line-regexp
  " at \\(.*\\):\\([0-9]+\\)$")

;;  Regular expression that describes trepan "frame" line.
;;  e.g.
;; --> #0 Rubinius::Scope#my_method at kernel/common/variable_scope.rb:134
;;     #1 main.__script__ at /tmp/fact.rb:1
(setf (gethash "frame" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat dbgr-trepan-frame-start-regexp " " ; note common trepan
			dbgr-trepan-frame-num-regexp " "   ; note common trepan
			"\\([A-Z_][a-zA-Z0-9_:]*\\)[#.]\\(.*\\)"
			dbgr-trepanx-frame-file-line-regexp
			)
       :num 1
       :file-group 5
       :line-group 6)
      )

(setf (gethash "font-lock-keywords" dbgr-trepanx-pat-hash)
      '(
	;; File name and line number. E.g. at /test/gcd.rb:6
        ;;                                 ---^^^^^^^^^^^^-^
	(" at \\(.*\\):\\([0-9]+\\)$"
	 (1 dbgr-file-name-face)
	 (2 dbgr-line-number-face))

	;; The frame number and first type name, if present.
	("^\\(-->\\|   \\)? #\\([0-9]+\\) \\([a-zA-Z_][a-zA-Z0-9_]*\\).*\\(\\([#.]\\)?\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?\\)?"
	 (2 dbgr-backtrace-number-face)
	 (3 font-lock-constant-face)        ; e.g. Object
	 (6 font-lock-function-name-face nil t))   ; t means optional

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
