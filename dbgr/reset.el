;; reset state on quit or restart
(require 'load-relative)
(require-relative-list '("buffer" "fringe" "cmdbuf" "helper" "srcbuf" "bp") 
		       "dbgr-")
(defun dbgr-reset (&optional opt-buffer)
  "Reset state prior to quitting or restarting"
  (interactive)
  (let* ((buf (or opt-buffer (current-buffer)))
	 (cmdbuf 
	  (cond ((dbgr-cmdbuf? buf) buf)
		((dbgr-get-cmdbuf-from-srcbuf buf))
		('t nil))))
    (if cmdbuf 
	(with-current-buffer cmdbuf
	  (dbgr-fringe-erase-history-arrows)
	  (let ((buf-list (dbgr-sget 'cmdbuf-info 'srcbuf-list)))
	    (dolist (buf buf-list buf-list)
	      (with-current-buffer buf
		(loc-changes-clear-buffer)
		(dbgr-fringe-erase-history-arrows)
		;; FIXME: iterate over specific breakpoints.
		(dbgr-bp-remove-icons (point-min) (point-max))
		)
	      (message "buffer %s" buf)
	      ))
	  (setq dbgr-cmdbuf-info nil)
	  )
      ;; else
      (error "Unable to find command buffer from %s" buf)
      ))
  )

(provide-me "dbgr-")
