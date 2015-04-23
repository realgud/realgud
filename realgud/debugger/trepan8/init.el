;; Copyright (C) 2015 Free Software Foundation, Inc

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

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "realgud-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:trepan8-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a trepan8 location generally shown
;; before a command prompt.
;; For example:
;;  -> (/tmp/fact.rb:1)
;;  -- (kernel/common/scope.rb:134 remapped /tmp/scope.rb:134)
(setf (gethash "loc" realgud:trepan8-pat-hash)
      (make-realgud-loc-pat
       :regexp (format ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):%s\\(?: @[0-9]+\\)?)"
		       realgud:regexp-captured-num)
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a trepan8 command prompt
;; For example:
;;   (trepan8):
;;   ((trepan8)):
(setf (gethash "prompt" realgud:trepan8-pat-hash)
      (make-realgud-loc-pat
       :regexp "^(+trepan8\\(@[0-9]+\\|@main\\)?)+: "
       ))

;;  Regular expression that describes a MRI 1.8 Ruby backtrace line.
(setf (gethash "lang-backtrace" realgud:trepan8-pat-hash)
      realgud-ruby-backtrace-loc-pat)

;;  Regular expression that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" realgud:trepan8-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;; Regular expression that describes a "breakpoint set" line
;; For example:
;; Set breakpoint 1: /tmp/fact.rb:1 (@0)
(setf (gethash "brkpt-set" realgud:trepan8-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Set breakpoint %s: .+ at \\(.+\\):\\([0-9]+\\) (@[0-9]+)"
		       realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:'
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:trepan8-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Deleted breakpoint %s.\n"
		       realgud:regexp-captured-num)
       :num 1))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" realgud:trepan8-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

(setf (gethash "trepan8" realgud-pat-hash) realgud:trepan8-pat-hash)

(defconst realgud:trepan8-frame-file-line-regexp
  (format " at \\(.*\\):%s$" realgud:regexp-captured-num))

(defconst realgud:trepan8-frame-start-regexp realgud:trepan-frame-start-regexp)
(defconst realgud:trepan8-frame-num-regexp   realgud:trepan-frame-start-regexp)

;;  Regular expression that describes a debugger "backtrace" command line.
;;  e.g.
;; --> #0 at line /usr/bin/irb:12
;;     #1 main.__script__ at /tmp/fact.rb:1
;;     #1 main.__script__ at /tmp/fact.rb:1
;;     #0 IRB.start(ap_path#String) at line /usr/lib/ruby/1.8/irb.rb:52
(setf (gethash "debugger-backtrace" realgud:trepan8-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:trepan8-frame-start-regexp " "
			realgud:trepan8-frame-num-regexp
			"\\(?: \\(?:\\(.+\\)(\\(.*\\))\\)\\)?"
			realgud:trepan8-frame-file-line-regexp
			)
       :num 2
       :file-group 5
       :line-group 6)
      )

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud:trepan8-pat-hash)
       "^trepan8: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepan8-pat-hash)
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
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

(setf (gethash "trepan8" realgud-pat-hash) realgud:trepan8-pat-hash)

(defvar realgud:trepan8-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepan8 command to use, like 'quit!'")

(setf (gethash "quit" realgud:trepan8-command-hash) "quit!")
(setf (gethash "shell" realgud:trepan8-command-hash) "irb")
(setf (gethash "trepan8" realgud-command-hash) realgud:trepan8-command-hash)

(provide-me "realgud:trepan8-")
