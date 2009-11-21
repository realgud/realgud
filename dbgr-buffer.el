(require 'load-relative)
(require-relative-list
 '("dbgr-fringe" "dbgr-cmdbuf" "dbgr-helper" "dbgr-srcbuf"))

(defvar dbgr-cmdbuf-info)

(defun dbgr-get-cmdbuf-from-srcbuf ( &optional opt-buffer)
  "Return the command buffer associated with source
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a source file buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (dbgr-srcbuf? buffer)
	(with-current-buffer-safe buffer
	  (dbgr-sget 'srcbuf-info 'cmdproc))
      nil)))

(defun dbgr-get-srcbuf-from-cmdbuf ( &optional opt-buffer)
  "Return the source buffer associated with command
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a process command buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (dbgr-cmdbuf? buffer)
	(with-current-buffer-safe buffer
	  (let ((loc (dbgr-loc-hist-item (dbgr-cmdbuf-info-loc-hist dbgr-cmdbuf-info))))
	    (if loc
		(marker-buffer (dbgr-loc-marker loc))
	      nil)
	    ))
      nil)))

(defun dbgr-get-srcbuf( &optional opt-buffer)
  "Return the script buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer-safe buffer
      (cond 
       ;; Perhaps buffer is a source script buffer?
       ((dbgr-srcbuf? buffer) buffer)
       ;; Perhaps buffer is a process command buffer.
       ((dbgr-cmdbuf? buffer)
	(dbgr-get-srcbuf-from-cmdbuf buffer))
       (t nil)))))

(defun dbgr-get-cmdbuf( &optional opt-buffer)
  "Return the script buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer-safe buffer
      (cond 
       ;; Perhaps buffer is a process command buffer?
       ((dbgr-cmdbuf? buffer) buffer)
       ;; Perhaps buffer is a source script buffer.
       ((dbgr-srcbuf? buffer)
	(dbgr-get-cmdbuf-from-srcbuf buffer))
       (t nil)))))

(provide-me)
