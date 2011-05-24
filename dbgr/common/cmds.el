;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list  '("send") "dbgr-")
(require-relative-list  '("buffer/command") "dbgr-buffer-")

(declare-function dbgr-terminate &optional cmdbuf)

(defun dbgr-cmd-remap(arg cmd-name default-cmd-template key
			  &optional no-record? frame-switch? dbgr-prompts?)
  "Run debugger command CMD-NAME using DEFAULT-CMD-TEMPLATE
if none has been set in the command hash."
  (let ((buffer (current-buffer))
	(cmdbuf (dbgr-get-cmdbuf))
	(cmd-hash)
	(cmd)
	)
    (with-current-buffer-safe cmdbuf
      (dbgr-cmdbuf-info-in-srcbuf?= (not (dbgr-cmdbuf? buffer)))
      (setq cmd-hash (dbgr-cmdbuf-info-cmd-hash dbgr-cmdbuf-info))
      (unless (and cmd-hash (setq cmd (gethash cmd-name cmd-hash)))
	(setq cmd default-cmd-template))
      )
    (dbgr-command cmd arg no-record? frame-switch? dbgr-prompts?)
    ;; FIXME: Figure out how to update the position if the source
    ;; buffer is displayed.
    ;; (if frame-switch?
    ;; 	(let* ((src-buffer (dbgr-get-srcbuf-from-cmdbuf cmdbuf))
    ;; 	       (src-window (get-buffer-window src-buffer))
    ;; 	       ))
    ;; 	  (with-selected-window src-window
    ;; 	    (message "recentering...")
    ;; 	    (dbgr-recenter-arrow)
    ;; 	  ))
    )
  ;; FIXME: this is a one-time thing. Put in caller.
  (local-set-key (format "\C-c%s" key) 
		 (intern (format "dbgr-cmd-%s" cmd-name)))
  )

(defun dbgr-cmd-backtrace(arg)
  "Show debugger breakpoint at the current line"
  (interactive "p")
  (dbgr-cmd-remap arg "backtrace" "backtrace" "T")
  )

(defun dbgr-cmd-break(arg)
  "Set a breakpoint at the current line"
  (interactive "p")
  (dbgr-cmd-remap arg "break" "break %X:%l" "b")
  )

(defun dbgr-cmd-continue(&optional arg)
    "Continue execution."
    (interactive "MContinue args: ")
    (dbgr-cmd-remap arg "continue" "continue" "c")
)

(defun dbgr-cmd-eval(arg)
    "Exaluate an expression."
    (interactive "MEval expesssion: ")
    (dbgr-cmd-remap arg "eval" "eval %s" "e")
)

(defun dbgr-cmd-eval-region(start end)
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (dbgr-cmd-remap text "eval" "eval %s" "e")
      )
    )

(defun dbgr-cmd-finish(&optional arg)
    "Run until the completion of the current stack frame.

This command is often referred to as 'step out' as opposed to
'step over' or 'step into'.
"
    (interactive "p")
    (dbgr-cmd-remap arg "finish" "finish" ".")
)

(defun dbgr-cmd-frame(&optional arg)
    "Change the current frame number to the value of the numeric argument.
If no argument specified use 0 or the most recent frame."
    (dbgr-cmd-remap arg "frame" "frame %p" "f" t t)
)

(defun dbgr-cmd-kill(arg)
  "kill debugger process"
  (interactive "p")
  (dbgr-cmd-remap arg "kill" "kill" "k" nil nil 't)
  )

(defun dbgr-cmd-newer-frame(&optional arg)
    "Move the current frame to a newer (more recent) frame. 
With a numeric argument move that many levels forward."
    (interactive "p")
    (dbgr-cmd-remap arg "down" "down %p" "<" t t)
)

(defun dbgr-cmd-next(&optional arg)
    "Step one source line at current call level.  

With a numeric argument, step that many times. This command is
often referred to as 'step through' as opposed to 'step into' or
'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive "p")
    (dbgr-cmd-remap arg "next" "next %p" "n")
)

(defun dbgr-cmd-older-frame(&optional arg)
  "Move the current frame to an older (less recent) frame. 
With a numeric argument move that many levels back."
    (interactive "p")
    (dbgr-cmd-remap arg "up" "up %p" ">" t t)
)

(defun dbgr-cmd-restart(&optional arg)
    "Restart execution."
    (interactive "MRestart args: ")
    (dbgr-cmd-remap arg "restart" "run" "R" 't nil 't)
)

(defun dbgr-cmd-shell(&optional arg)
    "Restart execution."
    (interactive "p")
    (dbgr-cmd-remap arg "shell" "shell" "S")
)

(defun dbgr-cmd-step(&optional arg)
    "Step one source line. 

With a numeric argument, step that many times.
This command is often referred to as 'step into' as opposed to
'step over' or 'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive "p")
    (dbgr-cmd-remap arg "step" "step %p" "s")
)

(defun dbgr-cmd-quit (&optional arg)
  "Gently terminate execution of the debugged program."
  (interactive "p")
  (let ((buffer (current-buffer))
	(cmdbuf (dbgr-get-cmdbuf))
	(cmd-hash)
	(cmd)
	)
    (if cmdbuf
	(progn
	  (with-current-buffer cmdbuf
	    (dbgr-cmdbuf-info-in-srcbuf?= (not (dbgr-cmdbuf? buffer)))
	    (setq cmd-hash (dbgr-cmdbuf-info-cmd-hash dbgr-cmdbuf-info))
	    (unless (and cmd-hash (setq cmd (gethash "quit" cmd-hash)))
	      (setq cmd "quit"))
	    )
	  (dbgr-command cmd arg 't)
	  (if cmdbuf (dbgr-terminate cmdbuf))
	  )
      ; else
      (dbgr-terminate-srcbuf buffer)
      )
    )
  )

(local-set-key "\C-cq" 'dbgr-cmd-quit)

(provide-me "dbgr-")
