;; Copyright (C) 2010-2011, 2016, 2018 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Ruby 1.8 debugger: ruby-debug (rdebug)

(eval-when-compile (require 'cl-lib))   ;For stef.

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "realgud-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud-rdebug-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a rdebug location generally shown
;; before a command prompt.
;; For example:
;;  /usr/lib/ruby/1.8/rubygems/custom_require.rb:31  # in Emacs
;; /usr/bin/irb:12
(setf (gethash "loc" realgud-rdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(?:source \\)?\\(\\(?:[a-zA-Z]:\\)?\\(?:.+\\)\\):\\([0-9]+\\).*\\(?:\n\\|$\\)"
       :file-group 1
       :line-group 2
      ))

;; An initial list of regexps that don't generally have files
;; associated with them and therefore we should not try to find file
;; associations for them.  This list is used to seed a field of the
;; same name in the cmd-info structure inside a command buffer. A user
;; may add additional files to the command-buffer's re-ignore-list.
(setf (gethash "ignore-re-file-list" realgud-rdebug-pat-hash)
      '("(eval)"))

;; Regular expression that describes a rdebug command prompt
;; For example:
;;   (rdb:1)
(setf (gethash "prompt" realgud-rdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "^(rdb:[0-9]+) "
       ))

;;  Regular expression that describes a Ruby backtrace line.
(setf (gethash "lang-backtrace" realgud-rdebug-pat-hash)
      realgud-ruby-backtrace-loc-pat)

;;  Regular expression that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" realgud-rdebug-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;; Regular expression that describes a rdebug "breakpoint set" line
;; For example:
;;   Breakpoint 1 file /test/gcd.rb, line 6
;;   -----------^------^^^^^^^^^^^^-------^
(setf (gethash "brkpt-set" realgud-rdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) file \\(.+\\), line \\([0-9]+\\)\n"
       :num 1
       :file-group 2
       :line-group 3))

(defconst realgud-rdebug-frame-file-line-regexp
  "[ \t\n]+at line \\(.*\\):\\([0-9]+\\)$")

(defconst realgud-rdebug-frame-start-regexp realgud:trepan-frame-start-regexp)
(defconst realgud-rdebug-frame-num-regexp   realgud:trepan-frame-num-regexp)

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" realgud-rdebug-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;;  Regular expression that describes a Ruby $! string
(setf (gethash "rails-backtrace" realgud-rdebug-pat-hash)
      realgud-rails-backtrace-loc-pat)

;;  Regular expression that describes a debugger "backtrace" command line.
;;  e.g.
;; --> #0 at line /usr/bin/irb:12
;;     #1 main.__script__ at /tmp/fact.rb:1
;;     #1 main.__script__ at /tmp/fact.rb:1
;;     #0 IRB.start(ap_path#String) at line /usr/lib/ruby/1.8/irb.rb:52
(setf (gethash "debugger-backtrace" realgud-rdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud-rdebug-frame-start-regexp " "
			realgud-rdebug-frame-num-regexp
			"\\(?: \\(?:\\(.+\\)(\\(.*\\))\\)\\)?"
			realgud-rdebug-frame-file-line-regexp
			)
       :num 2
       :file-group 5
       :line-group 6)
      )

(setf (gethash "font-lock-keywords" realgud-rdebug-pat-hash)
      '(
	;; Parameters and first type entry. E.g Object.gcd(a#Fixnum, b#Fixnum)
	;;                                                 ^-^^^^^^  ^-^^^^^^
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)#\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>"
	 (1 font-lock-variable-name-face)
	 (2 font-lock-constant-face))

	;; "::Type", which occurs in class name of function and in
	;; parameter list.
	("::\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face))

	;; The frame number and first type name, if present.
	;; E.g. --> #0 Object.gcd(a#Fixnum, b#Fixnum)
        ;;      -----^-^^^^^^.^^^
	("^\\(-->\\)? *#\\([0-9]+\\) *\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[.:]\\)?"
	 (2 realgud-backtrace-number-face)
	 (4 font-lock-constant-face nil t))     ; t means optional.

	;; File name and line number. E.g. at line /test/gcd.rb:6
        ;;                                 -------^^^^^^^^^^^^^-^
	("at line \\(.*\\):\\([0-9]+\\)$"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))

	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (rdebug-frames-match-current-line
	;;  (0 rdebug-frames-current-frame-face append))
	))


(setf (gethash "rdebug" realgud-pat-hash) realgud-rdebug-pat-hash)

(defvar realgud-rdebug-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepanx command to use, like 'quit!'")

(setf (gethash "quit" realgud-rdebug-command-hash) "quit!")
(setf (gethash "shell" realgud-rdebug-command-hash) "irb")
(setf (gethash "rdebug" realgud-command-hash) realgud-rdebug-command-hash)

(provide-me "realgud-rdebug-")
