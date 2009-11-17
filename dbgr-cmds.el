(require 'load-relative)
(require-relative-list
 '("dbgr-send"))

(defun dbgr-define-gdb-like-commands ()
  "Define a bunch of gdb-command that we expect most debuggers to have"
  (dbgr-define-command 
      'break "break %f:%l" "\C-b" "Set a breakpoint at the current line")
  (dbgr-define-command 
      'step "step %p" "\C-s" "Step one source line")
  (dbgr-define-command 
      'next "next %p" "\C-s" "Step one source line at current call level")
)
