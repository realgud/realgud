;;; dbgr-locr.el --- Debugger location
;;; Commentary:

;; This describes a debugger location structure and has code for
;; working with them.

(eval-when-compile (require 'cl))

(defstruct dbgr-loc 
"Our own location type. Even though a mark contains a
file-name (via a buffer) and a line number (via an offset), we
want to save the values that were seen/requested originally."
   (filename    :type string)
   (line-number :type integer)
   (marker      :type marker))

(defalias 'dbgr-loc? 'dbgr-loc-p)

(defun dbgr-loc-current()
  "Create a location object for the point in the current buffer."
  (make-dbgr-loc :filename (buffer-file-name (current-buffer))
		  :line-number (line-number-at-pos) 
		  :marker (point-marker)))

(defun dbgr-loc-marker=(loc marker)
  (setf (dbgr-loc-marker loc) marker))

(defun dbgr-loc-goto(loc &optional window-fn &rest args)
  "Goto the LOC which may involve switching buffers and moving
the point to the places indicated by LOC. In the process, the buffer
and marker inside loc may be updated. If WINDOW-FN and ARGS are given,
WINDOW-FN is called before switching buffers"
  (if (dbgr-loc-p loc) 
      (lexical-let* ((filename    (dbgr-loc-filename loc))
		     (line-number (dbgr-loc-line-number loc))
		     (marker      (dbgr-loc-marker loc))
		     (buffer      (marker-buffer (or marker (make-marker)))))
	(if (not buffer)
	    (setq buffer (find-file-noselect filename)))
	(if buffer
	    (progn 
	      (if window-fn (apply window-fn args))
	      (switch-to-buffer buffer)
	      (if (and marker (marker-position marker))
		  (goto-char (marker-position marker))
		(progn 
		  (goto-char (point-min))
		  (forward-line (- line-number 1))
		  (dbgr-loc-marker= loc (point-marker))))))
	)))

(provide 'dbgr-loc)
