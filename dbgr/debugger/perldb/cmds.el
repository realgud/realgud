;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send" "../../common/track") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-perldb-commands ()
  "(Re)define a bunch of trepan commands"
  (dbgr-define-gdb-like-commands)
)

(provide-me "dbgr-perldb-")
