;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Common Python constants and regular expressions.
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track") 
		       "dbgr-")


(defconst dbgr-python-backtrace-loc-pat
  (make-dbgr-loc-pat
   :regexp "^[ \t]+File \"\\(.+\\)\", line \\([0-9]+\\)"
   :file-group 1
   :line-group 2)
  "A dbgr-loc-pat struct that describes a Python backtrace (or
traceback) line."  )

(defun dbgr-python-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{dbgr-example-map-standard}"
  (define-key map (kbd "C-c !b") 'dbgr-goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'dbgr-goto-lang-backtrace-line)
  )

(provide-me "dbgr-lang-")
