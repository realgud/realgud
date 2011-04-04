;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-kshdb-commands ()
  "(Re)define a bunch of kshdb commands"
  ;; kshdb doesn't allow for the more general file:line breakpoint yet.
  (dbgr-define-gdb-like-commands)
  (dbgr-define-command 
      'break "break %l" "\C-b" 
      "Set a breakpoint at the current line" t nil)
  )

(defvar dbgr-zshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepan command to use, like 'quit!'")

(setf (gethash "quit" dbgr-zshdb-command-hash) "quit!")
(setf (gethash "zshdb" dbgr-command-hash dbgr-zshdb-command-hash))

(provide-me "dbgr-kshdb-")
