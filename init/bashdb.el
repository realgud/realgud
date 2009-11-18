;;; init/bashdb.el --- Bash debugger bashdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative "../dbgr-regexp")
(require-relative "../dbgr-loc")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar bashdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a bashdb location generally shown
;; before a command prompt.
(setf (gethash "loc" bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

(setf (gethash "prompt" bashdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^bashdb<[(]*[0-9]+[)]*> "
       ))

(setf (gethash "bashdb" dbgr-pat-hash) bashdb-pat-hash)

;;; Local variables:
;;; eval:(put 'bashdb-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; init/bashdb.el ends here
