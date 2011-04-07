;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-remake-commands ()
  "(Re)define a bunch of remake commands"
  (dbgr-define-gdb-like-commands)
)

(provide-me "dbgr-remake-")
