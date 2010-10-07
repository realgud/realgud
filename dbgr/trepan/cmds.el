(require 'load-relative)
(require-relative-list
 '("../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-trepan-commands ()
  "(Re)define a bunch of trepan commands have"
  ;; trepan doesn't allow for the more general file:line breakpoint yet.
  (dbgr-define-gdb-like-commands)
  (dbgr-define-command 
      'break "break %l" "\C-b" 
      "Set a breakpoint at the current line" t nil)
  )

  (defun dbgr-cmd-quit (arg)
    "Gently terminate execution of the debugged program."
    (interactive "p")
    (let ((buffer (current-buffer))
	  (cmdbuf (dbgr-get-cmdbuf)))
      (with-current-buffer-safe cmdbuf
	(dbgr-cmdbuf-info-in-srcbuf?= dbgr-cmdbuf-info 
				      (not (dbgr-cmdbuf? buffer))))
      (dbgr-command "quit!" arg 't, 't)
      (if cmdbuf (dbgr-terminate cmdbuf))
      )
    )
  (local-set-key "\C-cq" 'dbgr-cmd-quit)


(provide-me "trepan-")
