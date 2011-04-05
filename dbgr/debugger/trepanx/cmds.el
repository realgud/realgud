;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-trepanx-commands ()
  "(Re)define a bunch of trepanx commands have"
  (dbgr-define-gdb-like-commands)
  )

(provide-me "dbgr-trepanx-")
