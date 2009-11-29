(require 'load-relative)
(require-relative-list
 '("send" "cmdbuf") "dbgr-")

(defun dbgr-define-gdb-like-commands ()
  "Define a bunch of gdb-command that we expect most debuggers to have"
  (dbgr-define-command 
      'break "break %f:%l" "\C-b" "Set a breakpoint at the current line")

  (dbgr-define-command 
      'step "step %p" "\C-s" 
      "Step one source line. With a numeric argument, step that many times.
This command is often referred to as 'step into' as opposed to
'step over' or 'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped.")

  (dbgr-define-command 
      'next "next %p" "\C-n" 
      "Step one source line at current call level. With a numeric
argument, step that many times. This ocmmand is often referred to as 'step over' as
opposed to 'step into' or 'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped.")

  (dbgr-define-command 
      'newer-frame "up %p" "<" 
"Up N stack frames (numeric arg).")

  (dbgr-define-command 
      'older-frame "down %p" ">" "Down N stack frames (numeric arg).")
  (dbgr-define-command 
      'frame "frame %p" "C-f" "Set frame to N (numeric arg).")
  (dbgr-define-command 
      'continue "continue" "C-r" "Continue execution.")
)

(provide-me "dbgr-")
