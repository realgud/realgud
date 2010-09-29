;;; pydbgr: Python 2.5 and beyond 

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar pydbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a pydbgr location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   (/usr/bin/zonetab2pot.py:15): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
;;  and in tracebacks like this:
;;   (/usr/bin/zonetab2pot.py:15)
(setf (gethash "loc" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^(Pydbgr) "
       ))

;;  Regular expression that describes a Python backtrace line.
(setf (gethash "backtrace" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^[ \t]+File \"\\(.+\\)\", line \\([0-9]+\\)"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+of file \\(.+\\)\\(\n\\|$\\)"
       :bp-num 1
       :file-group 3
       :line-group 2))

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :bp-num 1))

(setf (gethash "pydbgr" dbgr-pat-hash) pydbgr-pat-hash)

(provide-me "dbgr-init-")

;;; Local variables:
;;; eval:(put 'pydbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; regexp/pydbgr.el ends here
