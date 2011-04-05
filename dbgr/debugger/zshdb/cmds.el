;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defvar dbgr-zshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepan command to use, like 'quit!'")

(setf (gethash "quit" dbgr-zshdb-command-hash) "quit!")
(setf (gethash "zshdb" dbgr-command-hash dbgr-zshdb-command-hash))

(local-set-key "\C-cq" 'dbgr-cmd-quit)

(provide-me "dbgr-zshdb-")
