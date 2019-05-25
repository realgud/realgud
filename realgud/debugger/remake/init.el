;; Copyright (C) 2011, 2014, 2016, 2019 Free Software Foundation, Inc
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
;;; Regular expressions for GNU Make debugger: remake

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:remake-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

;; Top frame number
(setf (gethash "top-frame-num" realgud:remake-pat-hash) 0)

;; realgud-loc-pat that describes a remake location generally shown
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

;;  realgud-loc-pat that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:remake-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target \\([^:]*\\): file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 3
       :line-group 4))

;; realgud-loc-pat that describes a debugger "delete" (breakpoint) response.
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

;; realgud-loc-pat that describes a remake "backtrace" command line.
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

;; realgud-loc-pat that describes a debugger "backtrace" command line.
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

;; FIXME breakpoints aren't locations. It should be a different structure
;; Regular expression that describes a gdb "info breakpoint" line
;; For example:
;;  1 breakpoint     keep   y 0x07 ../../../config.status at /src/realgud/realgud/debugger/remake/Makefile:282
;;  2 breakpoint     keep   y 0x07 ../../../configure at /src/realgud/realgud/debugger/remake/Makefile:285
;;  3 breakpoint     keep   y 0x07 ../../../configure.ac at /src/realgud/realgud/debugger/remake/Makefile:289

(setf (gethash "debugger-breakpoint" realgud:remake-pat-hash)
  (make-realgud-loc-pat
   :regexp (format "^[ \t]*%s[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)[ \t]+\\([yn]\\)[ \t]+\\(0x??\\).* at \\(.+\\):%s"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :num 1
   :text-group 2  ;; misnamed Is "breakpoint" or "watchpoint"
   :string 3      ;; misnamed. Is "keep" or "del"
   ;; Skipped mask
   :file-group 6
   :line-group 7)
  )


(setf (gethash "font-lock-breakpoint-keywords" realgud:remake-pat-hash)
  '(
    ;;  1 breakpoint     keep   y 0x07 ../../../config.status at /src/external-vcs/github/rocky/realgud/realgud/debugger/remake/Makefile:282
    ;;  ^ ^^^^^^^^^^     ^^^^   ^
    ("^[ \t]+\\([0-9]+\\)[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)[ \t]+\\([yn]\\)"
     (1 realgud-breakpoint-number-face)
     (2 font-lock-function-name-face nil t)     ; t means optional.
     (3 font-lock-function-name-face nil t))    ; t means optional.

    ;;  1 breakpoint     keep   y 0x07 ../../../config.status at /src/realgud/realgud/debugger/remake/Makefile:282
    ;;                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
    (" \\(0x??\\) \\(.+\\) at \\(.+\\):\\([0-9]+\\)"
     (3 realgud-file-name-face)
     (4 realgud-line-number-face))
    ))

;; realgud-loc-pat that describes which frame is selected in
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
(setf (gethash "info-breakpoints" realgud:remake-command-hash) "info breakpoints")
(setf (gethash "remake" realgud-command-hash) realgud:remake-command-hash)

;; Unsupported features:
(setf (gethash "jump"  realgud:remake-command-hash) "*not-implemented*")

(provide-me "realgud:remake-")
