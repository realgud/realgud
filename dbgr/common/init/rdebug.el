;;; Ruby 1.8 debuggger: ruby-debug (rdebug)

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar rdebug-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
traceback, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a rdebug location generally shown
;; before a command prompt.
(setf (gethash "loc" rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:source \\)?\\(\\(?:[a-zA-Z]:\\)?\\(?:.+\\)\\):\\([0-9]+\\).*\\(?:\n\\|$\\)"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a rdebug command prompt
(setf (gethash "prompt" rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(rdb:[0-9]+) "
       ))

;;  Regular expression that describes a Ruby traceback line.
(setf (gethash "traceback" rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a rdebug "breakpoint set" line
(setf (gethash "brkpt-set" rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) file \\(.+\\), line \\([0-9]+\\)\n"
       :bp-num 1
       :file-group 2
       :line-group 3))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" rdebug-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))

(setf (gethash "rdebug" dbgr-pat-hash) rdebug-pat-hash)

(provide-me "dbgr-init-")
