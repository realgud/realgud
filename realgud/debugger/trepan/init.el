;; Copyright (C) 2010, 2014-2016, 2019 Free Software Foundation, Inc

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

;; Ruby Trepanning debugger
(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:trepan-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(setf (gethash "loc-callback-fn" realgud:trepan-pat-hash) 'realgud:trepan-loc-fn-callback)

;; Regular expression that describes a trepan location generally shown
;; before a command prompt.
;; For example:
;; -- (/tmp/linecache.rb:64)
;; C> (/tmp/eval.rb:2)
(setf (gethash "loc" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?)\\(?:\n\\(.*?\\)\n\\)?"
       :file-group 1
       :line-group 2
       :text-group 3
       ))

;; An initial list of regexps that don't generally have files
;; associated with them and therefore we should not try to find file
;; associations for them.  This list is used to seed a field of the
;; same name in the cmd-info structure inside a command buffer. A user
;; may add additional files to the command-buffer's re-ignore-list.
(setf (gethash "ignore-re-file-list" realgud:trepan-pat-hash)
      '("(eval: .*)"))

;; Regular expression that describes a trepan command prompt
;; For example:
;;   (trepan):
;;   ((trepan)):
;;   (trepan@main):
;;   (trepan@55):
(setf (gethash "prompt" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp "^(+trepan\\(@[0-9]+\\|@main\\)?)+: "
       ))

;; Regular expression that describes a Ruby YARV 1.9 syntax error line.
(setf (gethash "syntax-error" realgud:trepan-pat-hash)
      realgud-ruby-YARV-syntax-error-pat)

;; Regular expression that describes a Ruby YARV backtrace line.
;; For example:
;; 	from /ruby/gems/2.2.0/gems/fog-1.32.0/lib/fog/digitalocean.rb:1:in `<top (required)>'
;; 	from /Users/fog-1.32.0/lib/fog.rb:28:in `require'
(setf (gethash "lang-backtrace" realgud:trepan-pat-hash)
      realgud-ruby-backtrace-loc-pat)

;;  realgud-loc-pat that describes a ruby $! backtrace
(setf (gethash "dollar-bang-backtrace" realgud:trepan-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;; realgud-loc-pat that describes a "breakpoint set" line.
;; For example:
;;   Breakpoint 1 set at VM offset 2 of instruction sequence "require",
;;	line 29 in file <internal:lib/rubygems/custom_require>.
;;   Breakpoint 2 set at VM offset 29 of instruction sequence "<top /xx.rb>",
;;	line 64 in file /src/external-vcs/linecache/trunk/lib/linecache.rb.
(setf (gethash "brkpt-set" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at .*[\n\t ]+line \\([0-9]+\\)[ \t\n]+in file \\(.+\\)."
       :num 1
       :file-group 3
       :line-group 2))

;; realgud-loc-pat that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\).\n"
       :num 1))

(defconst realgud:trepan-selected-frame-indicator "-->"
"String that describes which frame is selected in a debugger
backtrace listing.")

(defconst realgud:trepan-frame-file-regexp
  "[ \t\n]+in file \\([^ \n]+\\)")

(defconst realgud:trepan-debugger-name "trepan" "Name of debugger")

;; Top frame number
(setf (gethash "top-frame-num" realgud:trepan-pat-hash) 0)

;; realgud-loc-pat that describes a debugger "selected" frame in in
;; a frame-motion command.
;; For example:
;; --> #1 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9
(setf (gethash "selected-frame" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp
       (format "^%s #\\([0-9]+\\) .*%s"
	       realgud:trepan-selected-frame-indicator
	       realgud:trepan-frame-file-regexp)
       :num 1))

(setf (gethash "control-frame" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp "^c:\\([0-9]+\\) p:\\([0-9]+\\) s:\\([0-9]+\\) b:\\([0-9]+\\) l:\\([0-9a-f]+\\) d:\\([0-9a-f]+\\) \\([A-Z]+\\) \\(.+\\):\\([0-9]+\\)"
       :file-group 8
       :line-group 9))

;;  realgud-loc-pat that describes a Ruby $! string
(setf (gethash "dollar-bang" realgud:trepan-pat-hash)
      realgud-ruby-dollar-bang-loc-pat)

;;  realgud-loc-pat that describes debugger "backtrace" command line.
;;  e.g.
;; --> #0 METHOD Object#require(path) in file <internal:lib/require> at line 28
;;     #1 TOP Object#<top /tmp/linecache.rb> in file /tmp/linecache.rb
(setf (gethash "debugger-backtrace" realgud:trepan-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:trepan-frame-start-regexp " "
			realgud:trepan-frame-num-regexp " "
			"\\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\(.*\\)"
			realgud:trepan-frame-file-regexp
			"\\(?:" realgud:trepan-frame-line-regexp "\\)?"
			)
       :num 2
       :file-group 6
       :line-group 7)
      )

;; realgud-loc-pat that for a termination message.
(setf (gethash "termination" realgud:trepan-pat-hash)
       "^trepan: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepan-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(-->\\|   \\)? #\\([0-9]+\\) \\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?"
	 (2 realgud-backtrace-number-face)
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
	 (1 realgud-file-name-face))
	;; Line number.
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 realgud-line-number-face))
	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

;; (setf (gethash "font-lock-keywords" realgud:trepan-pat-hash)
;;       '(
;; 	;; The frame number and first type name, if present.
;; 	((concat realgud:trepan-frame-start-regexp " "
;; 			realgud:trepan-frame-num-regexp " "
;; 			"\\([A-Z]+\\) *\\([A-Z_][a-zA-Z0-9_]*\\)[#]\\([a-zA-Z_][a-zA-Z_[0-9]]*\\)?")
;; 	 (2 realgud-backtrace-number-face)
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
;; 	(realgud:trepan-frame-file-regexp (1 realgud-file-name-face))
;; 	;; Line number.
;; 	(realgud:trepan-frame-line-regexp (1 realgud-line-number-face))
;; 	;; Function name.
;; 	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
;; 	 (1 font-lock-type-face)
;; 	 (2 font-lock-function-name-face))
;; 	;; (trepan-frames-match-current-line
;; 	;;  (0 trepan-frames-current-frame-face append))
;; 	))

(setf (gethash realgud:trepan-debugger-name realgud-pat-hash) realgud:trepan-pat-hash)

(defvar realgud:trepan-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepan command to use, like 'quit!'")

(setf (gethash "quit" realgud:trepan-command-hash) "quit!")
(setf (gethash "shell" realgud:trepan-command-hash) "irb")
(setf (gethash realgud:trepan-debugger-name
	       realgud-command-hash) realgud:trepan-command-hash)

(provide-me "realgud:trepan-")
