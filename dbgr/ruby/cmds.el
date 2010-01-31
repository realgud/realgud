(require 'load-relative)
(require-relative-list
 '("../send") "dbgr-")

(defun dbgr-define-rbdbgr-commands ()
  "(Re)define a bunch of rbdbgr commands have"
  ;; rbdbgr doesn't allow for the more general file:line breakpoint yet.
  (dbgr-define-command 
      'break "break %l" "\C-b" 
      "Set a breakpoint at the current line" t nil)
  )

(provide-me "rbdbgr-")


