;; Copyright (C) 2015-2016, 2019 Free Software Foundation, Inc

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

;; Trepanning Perl debugger
(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/perl") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:trepanpl-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a trepan.pl location generally shown
;; before a command prompt. We include matching the source text so we
;; can save that.
;;
;; Program-location lines look like this:
;; -- File::Basename::(/usr/share/perl/5.14/File/Basename.pm:284 @0x8918b70)
;; my $dirname = dirname(__FILE__);
;;
;; or for an eval'd expression:
;; -- main::((eval 1189)[/tmp/test.pl:2] remapped /tmp/JLlH.pl:1 @0xadcbda0)
;; $x = 1 + 2;
;;
;; or at a function call without the Perl OpCode position or source text:
;; -> main::(example/gcd.pl:8)

(setf (gethash "loc" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp (format ".. \\(?:.+::\\)?(\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):%s\\(?: @0x[0-9a-f]+\\)?)\\(?:\n\\(.*?\\)\n\\)?"
		       realgud:regexp-captured-num)
       :file-group 1
       :line-group 2
       :text-group 3
       :ignore-file-re  realgud-perl-ignore-file-re)
      )

(defconst realgud:trepanpl-frame-start-regexp
  "\\(^\\|\n\\)\\(?:-->\\|   \\) #")

;; Regular expression that describes a trepanpl command prompt
;; For example:
;;   (trepanpl):
;;   ((trepanpl)):
;;   (trepanpl@main):
;;   (trepanpl@55):
(defconst realgud:trepanpl-prompt-regexp
  "^(+trepanpl\\(@[0-9]+\\|@main\\)?)+: ")

(setf (gethash "prompt" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp realgud:trepanpl-prompt-regexp
       ))

(defconst realgud:trepanpl-eval-result-prefix-regexp
  "^\\$DB::D\\[[0-9]+\\] = ")

(setf (gethash "prompt" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp realgud:trepanpl-prompt-regexp
       ))


(defconst realgud:trepanpl-frame-num-regexp
  realgud:regexp-captured-num)

;; Regular expression that describes a Perl backtrace line.
;; For example:
;; --> #0 @ = File::Basename::fileparse('/usr/local/bin/trepan.pl') in
;;	file `/usr/share/perl/5.18.2/File/Basename.pm' at line 107
;;     #1 @ = File::Basename::dirname('/usr/local/bin/trepan.pl') in
;; 	file `/usr/share/perl/5.18.2/File/Basename.pm' at line 294
;;     #2 file `/usr/local/bin/trepan.pl' at line 11
(setf (gethash "debugger-backtrace" realgud:trepanpl-pat-hash)
  (make-realgud-loc-pat
   :regexp (concat
	    realgud:trepanpl-frame-start-regexp
	    realgud:trepanpl-frame-num-regexp
	    "\\(?: [$@] = .* in\\)?"
	    "[\n\t ]+?file `"
	    "\\(.*\\)' at line "
	    realgud:regexp-captured-num)
   :num 2
   :file-group 3
   :line-group 4
   :ignore-file-re  realgud-perl-ignore-file-re)
  )

;;  Regular expression that describes location in a Perl errmsg
(setf (gethash "perl-errmsg" realgud:trepanpl-pat-hash)
      realgud-perl-errmsg-loc-pat)

;;  Regular expression that describes a Perl Carp backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
(setf (gethash "lang-backtrace" realgud:trepanpl-pat-hash)
      realgud-perl-carp-loc-pat)

;; Regular expression that describes a "breakpoint set" line.
;; For example:
;;   Breakpoint 1 set in (eval 1177)[/Eval.pm:94] at line 5"
;;   Breakpoint 2 set in /tmp/File/Basename.pm at line 215
(setf (gethash "brkpt-set" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s set in[\n\t ]+\\(.+\\)[ \t\n]+at line \\([0-9]+\\)"
		       realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3
       :ignore-file-re  realgud-perl-ignore-file-re)
      )

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Deleted breakpoint 1.
(setf (gethash "brkpt-del" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Deleted breakpoint %s\n"
		       realgud:regexp-captured-num)
       :num 1))

;; Regular expression that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 disabled.
(setf (gethash "brkpt-disable" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s disabled"
		       realgud:regexp-captured-num)
       :num 1))

;; Regular expression that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint 4 enabled.
(setf (gethash "brkpt-enable" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s enabled"
		       realgud:regexp-captured-num)
       :num 1))

(defconst realgud:trepanpl-selected-frame-indicator "-->"
"String that describes which frame is selected in a debugger
backtrace listing.")

(defconst realgud:trepanpl-frame-file-regexp
  "[ \t\n]+in file \\([^ \n]+\\)")

(defconst realgud:trepanpl-debugger-name "trepan.pl" "Name of debugger")

;; Top frame number
(setf (gethash "top-frame-num" realgud:trepanpl-pat-hash) 0)

;; realgud-loc-pat that describes a debugger "selected" frame in in
;; a frame-motion command.
;; For example:
;; --> #1 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/ipl at line 9
(setf (gethash "selected-frame" realgud:trepanpl-pat-hash)
      (make-realgud-loc-pat
       :regexp
       (format "^%s #\\([0-9]+\\) .*%s"
	       realgud:trepanpl-selected-frame-indicator
	       realgud:trepanpl-frame-file-regexp)
       :num 1))

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud:trepanpl-pat-hash)
       "^trepan.pl: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepanpl-pat-hash)
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
	;; (trepanpl-frames-match-current-line
	;;  (0 trepanpl-frames-current-frame-face append))
	))

;; (setf (gethash "font-lock-keywords" realgud:trepanpl-pat-hash)
;;       '(
;; 	;; The frame number and first type name, if present.
;; 	((concat realgud:trepanpl-frame-start-regexp " "
;; 			realgud:trepanpl-frame-num-regexp " "
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
;; 	(realgud:trepanpl-frame-file-regexp (1 realgud-file-name-face))
;; 	;; Line number.
;; 	(realgud:trepanpl-frame-line-regexp (1 realgud-line-number-face))
;; 	;; Function name.
;; 	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
;; 	 (1 font-lock-type-face)
;; 	 (2 font-lock-function-name-face))
;; 	;; (trepanpl-frames-match-current-line
;; 	;;  (0 trepanpl-frames-current-frame-face append))
;; 	))

(setf (gethash "callback-eval-filter" realgud:trepanpl-pat-hash)
      'realgud:trepanpl-eval-filter-callback)

(setf (gethash realgud:trepanpl-debugger-name realgud-pat-hash) realgud:trepanpl-pat-hash)

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger

(setf (gethash "trepan.pl" realgud:variable-basename-hash) "realgud:trepanpl")

(defvar realgud:trepanpl-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the trepanpl command to use, like 'quit!'")

(setf (gethash "break"  realgud:trepanpl-command-hash) "break %x %l")
(setf (gethash "eval"   realgud:trepanpl-command-hash) "eval %s")
(setf (gethash "quit"   realgud:trepanpl-command-hash) "quit!")
(setf (gethash "until"   realgud:trepanpl-command-hash) "continue %l")
(setf (gethash realgud:trepanpl-debugger-name
	       realgud-command-hash) realgud:trepanpl-command-hash)

;; Unsupported features:
(setf (gethash "jump"  realgud:trepanpl-command-hash) "*not-implemented*")

(provide-me "realgud:trepanpl-")
