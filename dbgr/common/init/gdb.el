;;; gdb debugger

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar gdb-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
the values of a hash entry is a dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; regular expression that describes a gdb location generally shown
;; before a command prompt. NOTE: we assume annotate 1!
(setf (gethash "loc" gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):beg:0x\\([0-9a-f]+\\)"
       :file-group 1
       :line-group 2
       :char-offset-group 3))

(setf (gethash "prompt" gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^(gdb) "
       ))

;;  regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" gdb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) at 0x\\([0-9a-f]*\\): file \\(.+\\), line \\([0-9]+\\).\n"
       :bp-num 1
       :file-group 3
       :line-group 4))

(setf (gethash "gdb" dbgr-pat-hash) gdb-pat-hash)
