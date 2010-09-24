(require 'load-relative)
(require-relative-list
 '("../common/send") "dbgr-")

(defun dbgr-define-trepanx-commands ()
  "(Re)define a bunch of trepanx commands have"
  ;; trepanx doesn't allow for the more general file:line breakpoint yet.
  (dbgr-define-command 
      'break "break %l" "\C-b" 
      "Set a breakpoint at the current line" t nil)
  )

(provide-me "trepanx-")


