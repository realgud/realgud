;;; init/pydbgr.el --- Python 2.5 and beyond pydbgr

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative "../dbgr-regexp")
(require-relative "../dbgr-loc")

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

;;  Regular expression that describes a Python traceback line.
(setf (gethash "traceback" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^[ \t]+File \"\\(.+\\)\", line \\([0-9]+\\)"
       :file-group 1
       :line-group 2))

(setf (gethash "pydbgr" dbgr-pat-hash) pydbgr-pat-hash)

;;; Local variables:
;;; eval:(put 'pydbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; regexp/pydbgr.el ends here
