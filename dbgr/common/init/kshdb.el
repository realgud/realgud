;;; Korn Shell debugger kshdb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar kshdb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a kshdb location generally shown
;; before a command prompt.
(setf (gethash "loc" kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

(setf (gethash "prompt" kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^kshdb<[(]*[0-9]+[)]*> "
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" kshdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :bp-num 1
       :file-group 2
       :line-group 3))

(setf (gethash "kshdb" dbgr-pat-hash) kshdb-pat-hash)

(provide-me "dbgr-init-")

;;; Local variables:
;;; eval:(put 'kshdb-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; init/kshdb.el ends here
