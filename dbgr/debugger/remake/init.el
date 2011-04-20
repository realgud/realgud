;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for GNU Make debugger: remake 

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
		       "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-remake-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a remake location generally shown
;; before a command prompt.
;; For example:
;; -- (emacs-dbgr/dbgr/debugger/Makefile:168)
(setf (gethash "loc" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:^\\|\n\\)\\(?:.. \\)?(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;; For example: 
;;   remake<10>
;;   remake<<1>>
(setf (gethash "prompt" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^remake[<]+\\([0-9]+\\)[>]+ "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target \\([^:]*\\): file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 3
       :line-group 4))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target .* cleared\n"
       :num 1))

(defconst dbgr-remake-selected-frame-arrow "=>"
"String that describes which frame is selected in a debugger
backtrace listing.")
(defconst dbgr-remake-frame-arrow (format "\\(%s\\|  \\)" 
					  dbgr-remake-selected-frame-arrow))
(defconst dbgr-remake-frame-num-regexp
  "#\\([0-9]+\\)  ")

(defconst dbgr-remake-frame-file-regexp " at \\(.*\\):\\([0-9]+\\)")

;; Regular expression that describes a remake "backtrace" command line.
;; For example:
;; #0  Makefile.in at /tmp/Makefile:216
;; #1  Makefile at /tmp/Makefile:230
(setf (gethash "lang-backtrace" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat "^"
			dbgr-remake-frame-num-regexp
			"\\(.*\\)"
			dbgr-remake-frame-file-regexp
			)
       :num 1
       :file-group 3
       :line-group 4)
      )

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;; =>#0  Makefile.in at /tmp/Makefile:216
;;   #1  Makefile at /tmp/Makefile:230
(setf (gethash "debugger-backtrace" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp 	(concat "^"
			dbgr-remake-frame-arrow
			dbgr-remake-frame-num-regexp
			"\\(.*\\)"
			dbgr-remake-frame-file-regexp
			)
       :num 2
       :file-group 4
       :line-group 5)
      )

;; Regular expression that describes which frame is selected in 
;; a debugger backtrace listing.
(setf (gethash "selected-frame-indicator" dbgr-remake-pat-hash)
      dbgr-remake-selected-frame-arrow)

;; Regular expression for a termination message.
(setf (gethash "termination" dbgr-remake-pat-hash)
       "^remake: That's all, folks...\n")

(setf (gethash "font-lock-keywords" dbgr-remake-pat-hash)
      '(
	;; ;; File name and line number
	;; ;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;; ;;                       ----^^^^^^^^^^^^^^^^^
	(" at \\(.*\\):\\([0-9]+\\)"
	 (1 dbgr-file-name-face)
	 (2 dbgr-line-number-face))

	;; The frame number and first type name, if present.
	;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;;      ---^
	("#\\([0-9]+\\)  "
	 (1 dbgr-backtrace-number-face))
	))

(setf (gethash "remake" dbgr-pat-hash) dbgr-remake-pat-hash)

(defvar dbgr-remake-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the remake command to use, like 'q'")

(setf (gethash "break"  dbgr-remake-command-hash) "break %l")
(setf (gethash "eval"   dbgr-remake-command-hash) "expand %s")
(setf (gethash "remake" dbgr-command-hash) dbgr-remake-command-hash)


(provide-me "dbgr-remake-")
