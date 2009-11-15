(require 'load-relative)

;; FIXME DRY using a macro
(eval-when-compile 
  (require 'cl)
  (dolist (rel-file 
	   '("dbgr-scriptbuf" "dbgr-cmdbuf"
	     "dbgr-lochist" "dbgr-loc"))
    (require-relative rel-file)))
(dolist (rel-file 
	 '("dbgr-scriptbuf" "dbgr-cmdbuf"
	   "dbgr-lochist" "dbgr-loc"))
  (require-relative rel-file))

(defvar dbgr-cmdbuf-info)

(defun dbgr-cmdbuf-get-scriptbuf ( &optional opt-buffer)
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
	(dbgr-cmdbuf-get-scriptbuf buffer))
       (t nil)))))
