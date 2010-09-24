(require 'load-relative)
(require-relative-list
 '("../common/send") "dbgr-")

(defun dbgr-define-trepan-commands ()
  "(Re)define a bunch of trepan commands have"
  ;; trepan doesn't allow for the more general file:line breakpoint yet.
  (dbgr-define-command 
      'break "break %l" "\C-b" 
      "Set a breakpoint at the current line" t nil)
  )

(provide-me "trepan-")


