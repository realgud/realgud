(require 'load-relative)
(require-relative-list
 '("dbgr-arrow" "dbgr-cmdbuf" "dbgr-scriptbuf" "dbgr-track"))

(defvar dbgr-cmdbuf-info)

(defun dbgr-get-scriptbuf-from-cmdbuf ( &optional opt-buffer)
  "Return the script buffer associated with command
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a process command buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((loc (dbgr-loc-hist-item (dbgr-cmdbuf-loc-hist dbgr-cmdbuf-info))))
	(if loc
	    (marker-buffer (dbgr-loc-marker loc))
	  nil)
	)))
)

(defun dbgr-scriptbuf-get( &optional opt-buffer)
  "Return the script buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer buffer
      (cond 
       ;; Perhaps buffer is a script buffer.
       ((dbgr-scriptbuf? buffer) buffer)
       ;; Perhaps buffer is a process command buffer.
       ((dbgr-cmdbuf? buffer)
	(dbgr-get-scriptbuf-from-cmdbuf buffer))
       (t nil)))))
