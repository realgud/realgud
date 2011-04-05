;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-bashdb-commands ()
  "(Re)define a bunch of bashdb commands"
  (dbgr-define-gdb-like-commands)
  )

(defvar dbgr-bashdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the bashdb command to use, like 'quit!'")

(setf (gethash "quit" dbgr-bashdb-command-hash) "quit!")
(setf (gethash "bashdb" dbgr-command-hash dbgr-bashdb-command-hash))

(provide-me "dbgr-bashdb-")
