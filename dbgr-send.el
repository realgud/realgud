(require 'load-relative)
(require-relative-list
 '("dbgr-buffer"))

(defun dbgr-send-command (command &optional opt-buffer)
  "Invoke the debugger COMMAND displaying."
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (cmdbuf (dbgr-get-cmdbuf buffer)))
    (if cmdbuf
	(let ((proc (get-buffer-process cmdbuf)))
	  (or proc (error "Current buffer has no process"))
	  (apply comint-input-sender (list proc command))
	  ;;(process-send-string proc (concat command "\n"))
	  ))))

(provide 'dbgr-send)
