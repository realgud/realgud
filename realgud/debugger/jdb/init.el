;; Copyright (C) 2014-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")

(require-relative-list '("../../lang/java") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defconst realgud:jdb-identifier "[A-Za-z_][A-Za-z0-9_.]+"
"Regexp string that matches a Java identifier possily with class
name. For example java.lang.Class.getDeclaredMethods")

(defvar realgud:jdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(setf (gethash "loc-callback-fn" realgud:jdb-pat-hash)
      'realgud:jdb-loc-fn-callback)

;; realgud-loc-pat that describes a jdb location generally shown
;; before a command prompt. For example:
;;   Breakpoint hit: "thread=main", TestMe.main(), line=7 bci=0
;;   Step completed: "thread=main", TestMe.<init>(), line=15 bci=0

(setf (gethash "loc" realgud:jdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(?:Breakpoint hit\\|Step completed\\): \"thread=.+\", \\(.+\\)?[.]\\(.+\\)(), line=\\([0-9]+\\) bci=\\([0-9]+\\)\\(?:\n\\([0-9]+\\)\\(.*\\)\\)?"
       :file-group 1
       :line-group 3
       :text-group 6))

;; realgud-loc-pat that describes a jdb command prompt
;; For example:
;;   main[1]
;;   main[2]
;;   >
;; FIXME: I think the pattern is thread-name[stack-level]
;; Here, we only deal with main.
(setf (gethash "prompt" realgud:jdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^\\(?:main\\[\\([0-9]+\\)\\]\\|>\\) "
       :num 1
       ))

;; realgud-loc-pat that describes a Java syntax error line.
;; (setf (gethash "syntax-error" realgud:jdb-pat-hash)
;;       realgud-java-syntax-error-pat)

;; realgud-loc-pat that describes a Java backtrace line.
;; For example:
;;  [1] ca.snpEffect.commandLine.SnpEff.run (SnpEff.java:7)
(setf (gethash "lang-backtrace" realgud:jdb-pat-hash)
  (make-realgud-loc-pat
   ;; FIXME: use realgud:jdb-identifier
   :regexp "^\\(?:[	 ]*[\\[0-9\\]+]\\) \\([A-Za-z_.][A-Za-z0-9.]+\\) (\\([A-Za-z_.][A-Za-z0-9.]+\\):\\([0-9]+\\))"
   :file-group 1
   :line-group 2))

;; realgud-loc-pat that describes a "breakpoint set" line.
;; For example:
;;   Set breakpoint TestMe:7
(setf (gethash "brkpt-set" realgud:jdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Set breakpoint \\(.+\\):\\([0-9]+\\)"
       :num 1
       :line-group 2))

;; realgud-loc-pat that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed: breakpoint TestMe:7
(setf (gethash "brkpt-del" realgud:jdb-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Removed breakpoint \\(.+\\):\\([0-9]+\\)\n"
       :line-group 1))

(defconst realgud:jdb-selected-frame-indicator "-->"
"String that describes which frame is selected in a debugger
backtrace listing.")

(defconst realgud:jdb-frame-file-regexp
  "[ \t\n]+in file \\([^ \n]+\\)")

(defconst realgud:jdb-debugger-name "jdb" "Name of debugger")

;; Top frame number
(setf (gethash "top-frame-num" realgud:jdb-pat-hash) 0)

;; realgud-loc-pat that describes a debugger "selected" frame in
;; a frame-motion command.
;; For example:
;; --> #1 [1] TestMe.main (TestMe.java:7)
;; Rocky: sometimes I am not getting the frame indicator.
(setf (gethash "selected-frame" realgud:jdb-pat-hash)
      (make-realgud-loc-pat
       :regexp
       (format "^%s #\\([0-9]+\\) .*%s"
	       realgud:jdb-selected-frame-indicator
	       realgud:jdb-frame-file-regexp)
       :num 1))

;; realgud-loc-pat that describes a jdb backtrace line.
;; For example:
;;  [1] TestMe.main (TestMe.java:7)
;;  [2] java.lang.Class.privateGetDeclaredMethods (Class.java:2,570)
;;  [3] java.lang.Class.getMethod0 (Class.java:2,813)
;;  [4] java.lang.Class.getMethod (Class.java:1,663)
;;  [5] sun.launcher.LauncherHelper.getMainMethod (LauncherHelper.java:494)
;;  [6] sun.launcher.LauncherHelper.checkAndLoadMain (LauncherHelper.java:486)
(setf (gethash "debugger-backtrace" realgud:jdb-pat-hash)
  (make-realgud-loc-pat
   :regexp "^\\(?:[\t ]*[\\[[0-9]+\\] \\)\\([A-Za-z_.][A-Za-z0-9.]+\\):\\([0-9]+\\)"
   :file-group 1
   :line-group 2))

;;  Regular expression that describes location in a maven error
(setf (gethash "maven-error" realgud:jdb-pat-hash)
      realgud-maven-error-loc-pat)

(setf (gethash "font-lock-keywords" realgud:jdb-pat-hash)
      '(
	;; The frame number and first type name, if present.
	;; FIXME: use realgud:jdb-identifier
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
	;; (jdb-frames-match-current-line
	;;  (0 jdb-frames-current-frame-face append))
	))

;; (setf (gethash "font-lock-keywords" realgud:jdb-pat-hash)
;;       '(
;; 	;; The frame number and first type name, if present.
;; 	((concat realgud:jdb-frame-start-regexp " "
;; 			realgud:jdb-frame-num-regexp " "
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
;; 	(realgud:jdb-frame-file-regexp (1 realgud-file-name-face))
;; 	;; Line number.
;; 	(realgud:jdb-frame-line-regexp (1 realgud-line-number-face))
;; 	;; Function name.
;; 	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
;; 	 (1 font-lock-type-face)
;; 	 (2 font-lock-function-name-face))
;; 	;; (jdb-frames-match-current-line
;; 	;;  (0 jdb-frames-current-frame-face append))
;; 	))

;; realgud-loc-pat for a termination message.
(setf (gethash "termination" realgud:jdb-pat-hash)
       "^The application exited\n")

(setf (gethash realgud:jdb-debugger-name realgud-pat-hash) realgud:jdb-pat-hash)

(defvar realgud:jdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the jdb command to use, like 'quit!'")

(setf (gethash realgud:jdb-debugger-name
	       realgud-command-hash) realgud:jdb-command-hash)

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger

(setf (gethash "jdb" realgud:variable-basename-hash) "realgud:jdb")


(setf (gethash "backtrace"   realgud:jdb-command-hash) "where")

;; For these we need to deal with java classpaths.
;; Also jdb is pretty sucky when it comes to giving an prompt that
;; we can write a regex for. So we don't even know often when there
;; is a prompt!
(setf (gethash "break"       realgud:jdb-command-hash) "*not-implemented*")
(setf (gethash "clear"       realgud:jdb-command-hash) "*not-implemented*")
(setf (gethash "restart"     realgud:jdb-command-hash) "*not-implemented*")

(setf (gethash "info-breakpoints" realgud:jdb-command-hash) "clear")

(setf (gethash "continue"    realgud:jdb-command-hash) "cont")
(setf (gethash "finish"      realgud:jdb-command-hash) "step up")
(setf (gethash "up"          realgud:jdb-command-hash) "up\C-Mwhere")
(setf (gethash "down"        realgud:jdb-command-hash) "down\C-Mwhere")


(provide-me "realgud:jdb-")
