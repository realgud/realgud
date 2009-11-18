;;; init/remake.el --- GNU make debugger (remake) regular expressions

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative "../dbgr-regexp")
(require-relative "../dbgr-loc")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar remake-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a dbgr-dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a remake location generally shown
;; before a command prompt.
(setf (gethash "loc" remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:^\\|\n\\)(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^mdb<[0-9]+> "
       ))

(setf (gethash "remake" dbgr-pat-hash) remake-pat-hash)

;;; Local variables:
;;; eval:(put 'remake-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; init/remake.el ends here
