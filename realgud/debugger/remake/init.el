;;; Copyright (C) 2011, 2014 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for GNU Make debugger: remake

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:remake-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Regular expression that describes a remake location generally shown
;; before a command prompt.
;; For example:
;; -- (emacs-dbgr/realgud/debugger/Makefile:168)
(setf (gethash "loc" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp "\\(?:^\\|\n\\)\\(?:.. \\)?(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))\\(?:\n\\(.*?\\)\n\\)?"
       :file-group 1
       :line-group 2
       :text-group 3))

;; For example:
;;   remake<10>
;;   remake<<1>>
(setf (gethash "prompt" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^remake[<]+\\([0-9]+\\)[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target \\([^:]*\\): file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 3
       :line-group 4))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target .* cleared\n"
       :num 1))

(defconst realgud:remake-selected-frame-arrow "=>"
"String that describes which frame is selected in a debugger
backtrace listing.")
(defconst realgud:remake-frame-arrow (format "\\(%s\\|  \\)"
					  realgud:remake-selected-frame-arrow))
(defconst realgud:remake-frame-num-regexp
  "#\\([0-9]+\\)  ")

(defconst realgud:remake-frame-file-regexp " at \\(.*\\):\\([0-9]+\\)")

;; Regular expression that describes a remake "backtrace" command line.
;; For example:
;; #0  Makefile.in at /tmp/Makefile:216
;; #1  Makefile at /tmp/Makefile:230
(setf (gethash "lang-backtrace" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat "^"
			realgud:remake-frame-num-regexp
			"\\(.*\\)"
			realgud:remake-frame-file-regexp
			)
       :num 1
       :file-group 3
       :line-group 4)
      )

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;; =>#0  Makefile.in at /tmp/Makefile:216
;;   #1  Makefile at /tmp/Makefile:230
(setf (gethash "debugger-backtrace" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat "^"
			realgud:remake-frame-arrow
			realgud:remake-frame-num-regexp
			"\\(.*\\)"
			realgud:remake-frame-file-regexp
			)
       :num 2
       :file-group 4
       :line-group 5)
      )

;; Regular expression that describes which frame is selected in
;; a debugger backtrace listing.
(setf (gethash "selected-frame-indicator" realgud:remake-pat-hash)
      realgud:remake-selected-frame-arrow)

;; Regular expression for a termination message.
(setf (gethash "termination" realgud:remake-pat-hash)
       "^remake: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:remake-pat-hash)
      '(
	;; ;; File name and line number
	;; ;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;; ;;                       ----^^^^^^^^^^^^^^^^^
	(" at \\(.*\\):\\([0-9]+\\)"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))

	;; The frame number and first type name, if present.
	;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;;      ---^
	("#\\([0-9]+\\)  "
	 (1 realgud-backtrace-number-face))
	))

(setf (gethash "remake" realgud-pat-hash) realgud:remake-pat-hash)

(defvar realgud:remake-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the remake command to use, like 'q'")

(setf (gethash "break"  realgud:remake-command-hash) "break %l")
(setf (gethash "eval"   realgud:remake-command-hash) "expand %s")
(setf (gethash "remake" realgud-command-hash) realgud:remake-command-hash)


(provide-me "realgud:remake-")
