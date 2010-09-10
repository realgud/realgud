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
       :regexp "^(+rbdbgr\\(@[0-9]+\\|@main\\)?)+: "
       ))

;;  Regular expression that describes a Ruby backtrace line.
(setf (gethash "backtrace" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+in file \\(.+\\),\n"
       :bp-num 1
       :file-group 3
       :line-group 2))

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :bp-num 1))

(setf (gethash "control-frame" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^c:\\([0-9]+\\) p:\\([0-9]+\\) s:\\([0-9]+\\) b:\\([0-9]+\\) l:\\([0-9a-f]+\\) d:\\([0-9a-f]+\\) \\([A-Z]+\\) \\(.+\\):\\([0-9]+\\)"
       :file-group 8
       :line-group 9))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))

(setf (gethash "rbdbgr" dbgr-pat-hash) rbdbgr-pat-hash)

(provide-me "dbgr-init-")
