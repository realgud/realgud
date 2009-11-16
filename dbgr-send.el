(require 'load-relative)
(require-relative-list
 '("dbgr-buffer"))

(defvar dbgr-send-command-fn
  (function (lambda (command-str) 
	    (comint-goto-process-mark)
	    (insert command-str)
	    (comint-send-input))) ;; Note: comint specific!
  "Should be a function which takes one string parameter without
any trailing newline. The function will be applied in the debugger process
buffer.")

;; Here are some other possibilities for functions.
;; Comint-specific: doesn't insert input into the buffer which is
;; what gud-call does.
;;   (apply comint-input-sender (list proc command))
;;
;; Works on any process-oriented buffer, not just comint.
;;   (process-send-string (get-buffer-process (current-buffer))
;;                        (concat command "\n"))


(defun dbgr-send-command (command &optional opt-buffer)
  "Invoke the debugger COMMAND displaying."
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (cmdbuf (dbgr-get-cmdbuf buffer)))
    (if cmdbuf
	(with-current-buffer cmdbuf
	  (let ((proc (get-buffer-process cmdbuf)))
	    (or proc (error "Command process buffer is not running"))
	    (funcall dbgr-send-command-fn command)
	    ))
      (error "Can't find command process buffer")
      )))

(provide 'dbgr-send)
