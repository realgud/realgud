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
      'next "next %p" "\C-n" "Step one source line at current call level")
  (dbgr-define-command 
      'up "up %p" "<" "Up N stack frames (numeric arg).")
  (dbgr-define-command 
      'down "down %p" ">" "Down N stack frames (numeric arg).")
  (dbgr-define-command 
      'frame "frame %p" "C-f" "Set frame to N (numeric arg).")
  (dbgr-define-command 
      'continue "continue" "C-r" "Continue execution.")
)

(provide 'dbgr-cmds)
