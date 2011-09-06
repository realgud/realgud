;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Trepanning Perl debugger
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
		       "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-trepanpl-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a trepanpl location generally shown
;; before a command prompt.
;; For example: 
;; -- (/tmp/linecache.pl:64)
;; C> (/tmp/eval.pl:2)
(setf (gethash "loc" dbgr-trepanpl-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?)"
       :file-group 1
       :line-group 2))

;; Regular expression that describes a trepanpl command prompt
;; For example: 
;;   (trepanpl): 
;;   ((trepanpl)):
;;   (trepanpl@main):
;;   (trepanpl@55):
(setf (gethash "prompt" dbgr-trepanpl-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(+trepanpl\\(@[0-9]+\\|@main\\)?)+: "
       ))

;; Regular expression that describes a Ruby YARV 1.9 backtrace line.
;; For example:
;; <internal:lib/rubygems/custom_require>:29:in `require'
;; <internal:lib/rubygems/custom_require>:29:in `require'
;; /tmp/Rakefile:50:in `<top /src/external-vcs/laser/Rakefile>'
;;	from /usr/lib/ruby/gems/rspec/compatibility.pl:6:in `const_missing'
(setf (gethash "lang-backtrace" dbgr-trepanpl-pat-hash)
  (make-dbgr-loc-pat
   :regexp "^\\(?:[\t]from \\)?\\([^:]+\\):\\([0-9]+\\)\\(?:in `.*'\\)?"
   :file-group 1
   :line-group 2))

;; Regular expression that describes a "breakpoint set" line. 
;; For example: 
;;   Breakpoint 1 set at VM offset 2 of instruction sequence "require",
;;	line 29 in file <internal:lib/rubygems/custom_require>.
;;   Breakpoint 2 set at VM offset 29 of instruction sequence "<top /xx.pl>",
;;	line 64 in file /src/external-vcs/linecache/trunk/lib/linecache.pl.
(setf (gethash "brkpt-set" dbgr-trepanpl-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at .*[\n\t ]+line \\([0-9]+\\)[ \t\n]+in file \\(.+\\)."
       :num 1
       :file-group 3
       :line-group 2))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" dbgr-trepanpl-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

(defconst dbgr-trepanpl-selected-frame-indicator "-->"
"String that describes which frame is selected in a debugger
backtrace listing.")

(defconst dbgr-trepanpl-frame-file-regexp
  "[ \t\n]+in file \\([^ \n]+\\)")

(defconst dbgr-trepanpl-debugger-name "trepanpl" "Name of debugger")

;; Top frame number
(setf (gethash "top-frame-num" dbgr-trepanpl-pat-hash) 0)

;; Regular expression that describes a debugger "selected" frame in in 
;; a frame-motion command.
;; For example:
;; --> #1 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/ipl at line 9
(setf (gethash "selected-frame" dbgr-trepanpl-pat-hash)
      (make-dbgr-loc-pat
       :regexp 
       (format "^%s #\\([0-9]+\\) .*%s" 
	       dbgr-trepanpl-selected-frame-indicator
	       dbgr-trepanpl-frame-file-regexp)
       :num 1))

;; Regular expression that for a termination message.
(setf (gethash "termination" dbgr-trepanpl-pat-hash)
       "^trepanpl: That's all, folks...\n")

(setf (gethash "font-lock-keywords" dbgr-trepanpl-pat-hash)
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
	;; (trepanpl-frames-match-current-line
	;;  (0 trepanpl-frames-current-frame-face append))
	))

;; (setf (gethash "font-lock-keywords" dbgr-trepanpl-pat-hash)
;;       '(
;; 	;; The frame number and first type name, if present.
;; 	((concat dbgr-trepanpl-frame-start-regexp " " 
;; 			dbgr-trepanpl-frame-num-regexp " "
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
;; 	(dbgr-trepanpl-frame-file-regexp (1 dbgr-file-name-face))
;; 	;; Line number.
;; 	(dbgr-trepanpl-frame-line-regexp (1 dbgr-line-number-face))
;; 	;; Function name.
;; 	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
;; 	 (1 font-lock-type-face)
;; 	 (2 font-lock-function-name-face))
;; 	;; (trepanpl-frames-match-current-line
;; 	;;  (0 trepanpl-frames-current-frame-face append))
;; 	))

(setf (gethash dbgr-trepanpl-debugger-name dbgr-pat-hash) dbgr-trepanpl-pat-hash)

(defvar dbgr-trepanpl-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepanpl command to use, like 'quit!'")

(setf (gethash "quit" dbgr-trepanpl-command-hash) "quit!")
(setf (gethash dbgr-trepanpl-debugger-name
	       dbgr-command-hash) dbgr-trepanpl-command-hash)

(provide-me "dbgr-trepanpl-")
