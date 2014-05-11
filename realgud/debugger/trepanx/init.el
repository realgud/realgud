;;; Copyright (C) 2010-2011, 2014 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "realgud-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:trepanx-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a trepanx location generally shown
;; before a command prompt.
;; For example:
;;  -> (/tmp/fact.rb:1)
;;  -- (kernel/common/scope.rb:134 remapped /tmp/scope.rb:134)
(setf (gethash "loc" realgud:trepanx-pat-hash)
      (make-realgud-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?)"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a trepanx command prompt
;; For example:
;;   (trepanx):
;;   ((trepanx)):
(setf (gethash "prompt" realgud:trepanx-pat-hash)
      (make-realgud-loc-pat
       :regexp "^(+trepanx\\(@[0-9]+\\|@main\\)?)+: "
       ))

;;  Regular expression that describes a Rubinius backtrace line.
(setf (gethash "lang-backtrace" realgud:trepanx-pat-hash)
      realgud-rubinius-backtrace-loc-pat)

;;  Regular expression that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" realgud:trepanx-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;;  Regular expression that describes a Rubinius X-agent backtrace
;;  line.
(setf (gethash "rubinius-backtrace-Xagent" realgud:trepanx-pat-hash)
      realgud-rubinius-Xagent-backtrace-loc-pat)

;; Regular expression that describes a "breakpoint set" line
;; For example:
;; Set breakpoint 1: /tmp/fact.rb:1 (@0)
(setf (gethash "brkpt-set" realgud:trepanx-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Set breakpoint \\([0-9]+\\): .+ at \\(.+\\):\\([0-9]+\\) (@[0-9]+)"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:'
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:trepanx-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\).\n"
       :num 1))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" realgud:trepanx-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

(setf (gethash "trepanx" realgud-pat-hash) realgud:trepanx-pat-hash)

(defconst realgud:trepanx-frame-file-line-regexp
  " at \\(.*\\):\\([0-9]+\\)$")

(defconst realgud:trepanx-frame-start-regexp realgud:trepan-frame-start-regexp)
(defconst realgud:trepanx-frame-num-regexp   realgud:trepan-frame-start-regexp)

;;  Regular expression that describes a debugger "backtrace" command line.
;;  e.g.
;; --> #0 Rubinius::Scope#my_method at kernel/common/variable_scope.rb:134
;;     #1 main.__script__ at /tmp/fact.rb:1
(setf (gethash "debugger-backtrace" realgud:trepanx-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:trepanx-frame-start-regexp " "
			realgud:trepanx-frame-num-regexp " "
			"\\([A-Z_][a-zA-Z0-9_:]*\\)[#.]\\(.*\\)"
			realgud:trepanx-frame-file-line-regexp
			)
       :num 2
       :file-group 6
       :line-group 7)
      )

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud:trepanx-pat-hash)
       "^trepanx: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepanx-pat-hash)
      '(
	;; File name and line number. E.g. at /test/gcd.rb:6
        ;;                                 ---^^^^^^^^^^^^-^
	(" at \\(.*\\):\\([0-9]+\\)$"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))

	;; The frame number and first type name, if present.
	("^\\(-->\\|   \\)? #\\([0-9]+\\) \\([a-zA-Z_][a-zA-Z0-9_]*\\).*\\(\\([#.]\\)?\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?\\)?"
	 (2 realgud-backtrace-number-face)
	 (3 font-lock-constant-face)        ; e.g. Object
	 (6 font-lock-function-name-face nil t))   ; t means optional

	;; The frame number and first type name, if present.
	("^ +\\([0-9]+\\) *\\([a-zA-Z_][a-zA-Z0-9_]*\\).*\\(\\([#.]\\)?\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?\\)?"
	 (1 realgud-backtrace-number-face)
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

(setf (gethash "trepanx" realgud-pat-hash) realgud:trepanx-pat-hash)

(defvar realgud:trepanx-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepanx command to use, like 'quit!'")

(setf (gethash "quit" realgud:trepanx-command-hash) "quit!")
(setf (gethash "shell" realgud:trepanx-command-hash) "irb")
(setf (gethash "trepanx" realgud-command-hash) realgud:trepanx-command-hash)

(provide-me "realgud:trepanx-")
