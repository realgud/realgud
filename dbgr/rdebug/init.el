;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Ruby 1.8 debuggger: ruby-debug (rdebug)

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-rdebug-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
traceback, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a rdebug location generally shown
;; before a command prompt.
;; E.g. 
(setf (gethash "loc" dbgr-rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:source \\)?\\(\\(?:[a-zA-Z]:\\)?\\(?:.+\\)\\):\\([0-9]+\\).*\\(?:\n\\|$\\)"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a rdebug command prompt
(setf (gethash "prompt" dbgr-rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(rdb:[0-9]+) "
       ))

;;  Regular expression that describes a Ruby traceback line.
(setf (gethash "traceback" dbgr-rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a rdebug "breakpoint set" line
;;  E.g. Breakpoint 1 file /test/gcd.rb, line 6
;;       -----------^------^^^^^^^^^^^^-------^           
(setf (gethash "brkpt-set" dbgr-rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) file \\(.+\\), line \\([0-9]+\\)\n"
       :bp-num 1
       :file-group 2
       :line-group 3))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" dbgr-rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))

(setf (gethash "font-lock-keywords" dbgr-rdebug-pat-hash)
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
	 (2 dbgr-backtrace-number-face)
	 (4 font-lock-constant-face nil t))     ; t means optional.

	;; File name and line number. E.g. at line /test/gcd.rb:6
        ;;                                 -------^^^^^^^^^^^^^-^
	("at line \\(.*\\):\\([0-9]+\\)$"
	 (1 dbgr-file-name-face)
	 (2 dbgr-line-number-face))

	;; Function name.
	("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (rdebug-frames-match-current-line
	;;  (0 rdebug-frames-current-frame-face append))
	))


(setf (gethash "rdebug" dbgr-pat-hash) dbgr-rdebug-pat-hash)

(provide-me "dbgr-rdebug-")
