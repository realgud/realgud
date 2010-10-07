(require 'load-relative)
(require-relative-list
 '("../common/send") "dbgr-")

(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-rdebug-commands ()
  "(Re)define a bunch of rdebug commands have"
  (dbgr-define-gdb-like-commands)
  ;; (dbgr-define-command 
  ;;     'break "break %l" "\C-b" 
  ;;     "Set a breakpoint at the current line" t nil)
  )

(provide-me "rdebug-")


