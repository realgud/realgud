;;; init/rdbdbgr.el --- Ruby 1.9 rbdbgr debugger

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar rbdbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
traceback, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a rbdbgr location generally shown
;; before a command prompt.
(setf (gethash "loc" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a rbdbgr command prompt
(setf (gethash "prompt" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(rbdbgr): "
       ))

;;  Regular expression that describes a Ruby traceback line.
(setf (gethash "traceback" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))

(setf (gethash "rbdbgr" dbgr-pat-hash) rbdbgr-pat-hash)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; regexp/rbdbgr.el ends here
