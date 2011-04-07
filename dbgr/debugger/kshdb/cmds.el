;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-kshdb-commands ()
  "(Re)define a bunch of kshdb commands"
  (dbgr-define-gdb-like-commands)
  )

(defvar dbgr-kshdb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is 
  the trepan command to use, like 'quit!'")

(setf (gethash "quit" dbgr-kshdb-command-hash) "quit!")
(setf (gethash "kshdb" dbgr-command-hash dbgr-kshdb-command-hash))

;; Break can only handle line number right now.
(setf (gethash "break" dbgr-kshdb-command-hash) "break %l")

(provide-me "dbgr-kshdb-")
