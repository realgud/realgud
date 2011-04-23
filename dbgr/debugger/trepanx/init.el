;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
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
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?)"
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
(setf (gethash "lang-backtrace" dbgr-trepanx-pat-hash) 
      dbgr-rubinius-backtrace-loc-pat)

;;  Regular expression that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" dbgr-trepanx-pat-hash) 
      dbgr-ruby-dollar-bang-loc-pat)

;;  Regular expression that describes a Rubinius X-agent backtrace
;;  line. 
(setf (gethash "rubinius-backtrace-Xagent" dbgr-trepanx-pat-hash) 
      dbgr-rubinius-Xagent-backtrace-loc-pat)

;; Regular expression that describes a "breakpoint set" line
;; For example: 
;; Set breakpoint 1: /tmp/fact.rb:1 (@0)
(setf (gethash "brkpt-set" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Set breakpoint \\([0-9]+\\): .+ at \\(.+\\):\\([0-9]+\\) (@[0-9]+)"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:'
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\).\n"
       :num 1))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" dbgr-trepanx-pat-hash) 
      dbgr-ruby-dollar-bang-loc-pat)

(setf (gethash "trepanx" dbgr-pat-hash) dbgr-trepanx-pat-hash)

(defconst dbgr-trepanx-frame-file-line-regexp
  " at \\(.*\\):\\([0-9]+\\)$")

(defconst dbgr-trepanx-frame-start-regexp dbgr-trepan-frame-start-regexp)
(defconst dbgr-trepanx-frame-num-regexp   dbgr-trepan-frame-start-regexp)

;;  Regular expression that describes a debugger "backtrace" command line.
;;  e.g.
;; --> #0 Rubinius::Scope#my_method at kernel/common/variable_scope.rb:134
;;     #1 main.__script__ at /tmp/fact.rb:1
(setf (gethash "debugger-backtrace" dbgr-trepanx-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat dbgr-trepanx-frame-start-regexp " "
			dbgr-trepanx-frame-num-regexp " " 
			"\\([A-Z_][a-zA-Z0-9_:]*\\)[#.]\\(.*\\)"
			dbgr-trepanx-frame-file-line-regexp
			)
       :num 2
       :file-group 6
       :line-group 7)
      )

;; Regular expression that for a termination message.
(setf (gethash "termination" dbgr-trepanx-pat-hash)
       "^trepanx: That's all, folks...\n")

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

(setf (gethash "trepanx" dbgr-pat-hash) dbgr-trepanx-pat-hash)

(defvar dbgr-trepanx-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepanx command to use, like 'quit!'")

(setf (gethash "quit" dbgr-trepanx-command-hash) "quit!")
(setf (gethash "shell" dbgr-trepanx-command-hash) "irb")
(setf (gethash "trepanx" dbgr-command-hash) dbgr-trepanx-command-hash)

(provide-me "dbgr-trepanx-")
