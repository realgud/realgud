(require 'load-relative)
(require-relative-list
 '("send" "cmdbuf") "dbgr-")

(declare-function dbgr-terminate &optional cmdbuf)

;; Note dbgr-define-command docstrings may appear in menu help, but only
;; the first line will appears. So be careful about where to put line
;; breaks in the docstrings below.
(defun dbgr-define-gdb-like-commands ()
  "Define a bunch of gdb-command that we expect most debuggers to have"
  (dbgr-define-command 
      'break "break %X:%l" "\C-b" 
      "Set a breakpoint at the current line")

  (dbgr-define-command 
      'step "step %p" "s" 
      "Step one source line. 

With a numeric argument, step that many times.
This command is often referred to as 'step into' as opposed to
'step over' or 'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped.")

  (dbgr-define-command 
      'next "next %p" "n" 
      "Step one source line at current call level.  

With a numeric argument, step that many times. This command is
often referred to as 'step over' as opposed to 'step into' or
'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped.")

  (dbgr-define-command 
      'finish "finish" "F" 
      "Run until the completion of the current stack frame.

This command is often referred to as 'step out' as opposed to
'step over' or 'step into'.
")

  (dbgr-define-command 
      'newer-frame "down %p" "<" 
"Move the current frame to a newer (more recent) frame. 

With a numeric argument move that many levels forward." t t)

  (dbgr-define-command 
      'older-frame "up %p" ">" 
"Move the current frame to an older (less recent) frame. 

With a numeric argument move that many levels back." t t)

  (dbgr-define-command 
      'frame "frame %p" "f" 
"Change the current frame number to the value of the numeric argument.

If no argument specified use 0 or the most recent frame." t t)

  (dbgr-define-command 
      'continue "continue" "c" 
      "Continue execution.")

  (defun dbgr-cmd-quit (arg)
    "Gently terminate execution of the debugged program."
    (interactive "p")
    (let ((buffer (current-buffer))
	  (cmdbuf (dbgr-get-cmdbuf)))
      (with-current-buffer-safe cmdbuf
	(dbgr-cmdbuf-info-in-srcbuf?= dbgr-cmdbuf-info 
				      (not (dbgr-cmdbuf? buffer))))
      (dbgr-command "quit" arg 't)
      (if cmdbuf (dbgr-terminate cmdbuf))
      )
    )
  (local-set-key "\C-cq" 'dbgr-cmd-quit)


  (dbgr-define-command 
      'restart "run" "R" 
      "Restart execution."
      't nil 't)
)

(provide-me "dbgr-")
