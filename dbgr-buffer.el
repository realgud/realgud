(require 'load-relative)
(require-relative-list
 '("dbgr-arrow" "dbgr-cmdbuf" "dbgr-scriptbuf" "dbgr-track"))

(defvar dbgr-cmdbuf-info)

(defun dbgr-get-cmdbuf-from-scriptbuf ( &optional opt-buffer)
  "Return the command buffer associated with source
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a source file buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (dbgr-scriptbuf? buffer)
	(with-current-buffer buffer
	  (dbgr-scriptbuf-info-cmdproc dbgr-scriptbuf-info))
      nil)))

(defun dbgr-get-scriptbuf-from-cmdbuf ( &optional opt-buffer)
  "Return the source buffer associated with command
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a process command buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (dbgr-cmdbuf? buffer)
	(with-current-buffer buffer
	  (let ((loc (dbgr-loc-hist-item (dbgr-cmdbuf-loc-hist dbgr-cmdbuf-info))))
	    (if loc
		(marker-buffer (dbgr-loc-marker loc))
	      nil)
	    ))
      nil)))

(defun dbgr-get-scriptbuf( &optional opt-buffer)
  "Return the script buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer buffer
      (cond 
       ;; Perhaps buffer is a source script buffer?
       ((dbgr-scriptbuf? buffer) buffer)
       ;; Perhaps buffer is a process command buffer.
       ((dbgr-cmdbuf? buffer)
	(dbgr-get-scriptbuf-from-cmdbuf buffer))
       (t nil)))))

(defun dbgr-get-cmdbuf( &optional opt-buffer)
  "Return the script buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer buffer
      (cond 
       ;; Perhaps buffer is a process command buffer?
       ((dbgr-cmdbuf? buffer) buffer)
       ;; Perhaps buffer is a source script buffer.
       ((dbgr-scriptbuf? buffer)
	(dbgr-get-cmdbuf-from-scriptbuf buffer))
       (t nil)))))

(provide 'dbgr-buffer)
