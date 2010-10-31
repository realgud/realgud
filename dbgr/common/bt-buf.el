(require 'load-relative)
(require-relative-list
 '("send" "track" "cmdbuf") "dbgr-")

;: FIXME: not picked up from track. Why?
(defvar dbgr-track-divert-string nil)

(defun dbgr-bt-init ()
  (interactive)
  (let ((buffer (current-buffer))
  	(cmdbuf (dbgr-get-cmdbuf))
  	(process)
  	)
    (with-current-buffer-safe cmdbuf
      (setq process (get-buffer-process (current-buffer)))
      (dbgr-cmdbuf-info-in-srcbuf?= dbgr-cmdbuf-info 
    				   (not (dbgr-cmdbuf? buffer)))
      (dbgr-cmdbuf-info-divert-output?= dbgr-cmdbuf-info 't)
      (setq dbgr-track-divert-string nil)
      (dbgr-command "backtrace" nil nil 't)
      (while (and (eq 'run (process-status process))
		    (null dbgr-track-divert-string))
	  (sleep-for 0.001)
	  )
      ;; (message "+++4 %s" dbgr-track-divert-string)
      (let ((bt-buffer (get-buffer-create
			(format "*%s backtrace*" (buffer-name))))
	    (divert-string dbgr-track-divert-string)
	    )
	(with-current-buffer bt-buffer
	  (setq buffer-read-only nil)
	  (delete-region (point-min) (point-max))
	  (if divert-string (insert divert-string))
	  )
	)
    )
  )
)

