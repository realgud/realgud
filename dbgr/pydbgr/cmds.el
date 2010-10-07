(require 'load-relative)
(require-relative-list
 '("../common/send") "dbgr-")

(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-pydbgr-commands ()
  "(Re)define a bunch of pydbgr commands have."
  (dbgr-define-gdb-like-commands)
  )

(provide-me "pydbgr-")


