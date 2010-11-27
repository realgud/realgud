;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
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
;; For example: 
;; -- (/tmp/linecache.rb:64)
;; C> (/tmp/eval.rb:2)
(setf (gethash "loc" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;; Regular expression that describes a trepan command prompt
;; For example: 
;;   (trepan): 
;;   ((trepan)):
(setf (gethash "prompt" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(+trepan\\(@[0-9]+\\|@main\\)?)+: "
       ))

;; Regular expression that describes a Ruby backtrace line.
(setf (gethash "backtrace" dbgr-trepan-pat-hash) dbgr-ruby-backtrace-loc-pat)

;; Regular expression that describes a "breakpoint set" line. 
;; For example: 
;;   Breakpoint 1 set at VM offset 2 of instruction sequence "require",
;;	line 29 in file <internal:lib/rubygems/custom_require>.
;;   Breakpoint 2 set at VM offset 29 of instruction sequence "<top /xx.rb>",
;;	line 64 in file /src/external-vcs/linecache/trunk/lib/linecache.rb.
(setf (gethash "brkpt-set" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+in file \\(.+\\),\n"
       :num 1
       :file-group 3
       :line-group 2))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

(setf (gethash "control-frame" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^c:\\([0-9]+\\) p:\\([0-9]+\\) s:\\([0-9]+\\) b:\\([0-9]+\\) l:\\([0-9a-f]+\\) d:\\([0-9a-f]+\\) \\([A-Z]+\\) \\(.+\\):\\([0-9]+\\)"
       :file-group 8
       :line-group 9))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" dbgr-trepan-pat-hash) dbgr-ruby-dollar-bang)

(defconst dbgr-trepan-frame-file-regexp
  "[ \t\n]+in file \\(.+\\)\\(?:[ \n]?\\|$\\)")

;;  Regular expression that describes debugger "backtrace" command line.
;;  e.g.
;; --> #0 METHOD Object#require(path) in file <internal:lib/require> at line 28
;;     #1 TOP Object#<top /tmp/linecache.rb> in file /tmp/linecache.rb
(setf (gethash "frame" dbgr-trepan-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat dbgr-trepan-frame-start-regexp " "
			dbgr-trepan-frame-num-regexp " "
			"\\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\(.*\\)"
			dbgr-trepan-frame-file-regexp
			"\\(?:" dbgr-trepan-frame-line-regexp "\\)?"
			)
       :num 1
       :file-group 5
       :line-group 6)
      )

(setf (gethash "font-lock-keywords" dbgr-trepan-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(-->\\|   \\)? #\\([0-9]+\\) \\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?"
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

;; (setf (gethash "font-lock-keywords" dbgr-trepan-pat-hash)
;;       '(
;; 	;; The frame number and first type name, if present.
;; 	((concat dbgr-trepan-frame-start-regexp " " 
;; 			dbgr-trepan-frame-num-regexp " "
;; 			"\\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?")
;; 	 (2 dbgr-backtrace-number-face)
;; 	 (3 font-lock-keyword-face)         ; e.g. METHOD, TOP
;; 	 (4 font-lock-constant-face)        ; e.g. Object
;; 	 (5 font-lock-function-name-face nil t))   ; t means optional
;; 	;; Instruction sequence
;; 	("<\\(.+\\)>"
;; 	 (1 font-lock-variable-name-face))
;; 	;; "::Type", which occurs in class name of function and in
;; 	;; parameter list.  Parameter sequence
;; 	("(\\(.+\\))"
;; 	 (1 font-lock-variable-name-face))
;; 	;; "::Type", which occurs in class name of function and in
;; 	;; parameter list.
;; 	("::\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
;; 	 (1 font-lock-type-face))
;; 	;; File name.
;; 	(dbgr-trepan-frame-file-regexp (1 dbgr-file-name-face))
;; 	;; Line number.
;; 	(dbgr-trepan-frame-line-regexp (1 dbgr-line-number-face))
;; 	;; Function name.
;; 	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
;; 	 (1 font-lock-type-face)
;; 	 (2 font-lock-function-name-face))
;; 	;; (trepan-frames-match-current-line
;; 	;;  (0 trepan-frames-current-frame-face append))
;; 	))

(setf (gethash "trepan" dbgr-pat-hash) dbgr-trepan-pat-hash)

(provide-me "dbgr-trepan-")
