;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Bash shell debugger: bashdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" "../../common/loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-bashdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a bashdb location generally shown
;; before a command prompt.
(setf (gethash "loc" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

(setf (gethash "prompt" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^bashdb<[(]*[0-9]+[)]*> "
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

(setf (gethash "font-lock-keywords" dbgr-bashdb-pat-hash)
      '(
	;; The frame number and first type name, if present.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;      --^-
	("^\\(->\\|##\\)\\([0-9]+\\) "
	 (2 dbgr-backtrace-number-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;          ---------^^^^^^^^^^^^^^^^^^^^-
	("[ \t]+\\(in\\|from\\) file `\\(.+\\)'"
	 (2 dbgr-file-name-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;                                         --------^^
	;; Line number.
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 dbgr-line-number-face))
	;; (trepan-frames-match-current-line
	;;  (0 trepan-frames-current-frame-face append))
	))

(setf (gethash "bashdb" dbgr-pat-hash) dbgr-bashdb-pat-hash)

(provide-me "dbgr-bashdb-")

