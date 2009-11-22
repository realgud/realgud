(require 'load-relative)
(require-relative-list
 '("dbgr-send" "dbgr-cmdbuf"))

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

;; FIXME: roll in-srcbuf option into dbgr-define-command and remove the
;; below
(defun dbgr-cmd-step (count)
  "Step one source line. With a numeric argument, step that many times.
This command is often referred to as 'step into' as opposed to
'step over' or 'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
  (interactive "p")
  (let ((buffer (current-buffer))
	(cmdbuf (dbgr-get-cmdbuf)))
    (with-current-buffer-safe cmdbuf
      (dbgr-cmdbuf-info-in-srcbuf= dbgr-cmdbuf-info 
				   (not (dbgr-cmdbuf? buffer))))
    (dbgr-command "step" count))
)

(provide-me)
