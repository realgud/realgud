(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")

(defun setup-bt-vars(debugger-name)
  "Sets up globals temp-cmdbuf and temp-bt with command buffer
for DEBUGGER-NAME"
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (setq temp-bt (generate-new-buffer "*bt-test*"))
  (with-current-buffer temp-cmdbuf
    (switch-to-buffer temp-cmdbuf)
    (dbgr-cmdbuf-init temp-cmdbuf debugger-name
		      (gethash debugger-name dbgr-pat-hash))
  
    (switch-to-buffer nil)
  ))


(defun setup-bt(debugger-name string)
  "Sets up globals temp-cmdbuf and temp-bt with command buffer
for DEBUGGER-NAME and initializes it to STRING"
  (setup-bt-vars debugger-name)
  (with-current-buffer temp-bt
    (dbgr-backtrace-mode temp-cmdbuf)
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (insert string)
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    )
  temp-bt
)

