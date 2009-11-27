;;; Debugger location
;;; Commentary:

;; This describes a debugger location structure and has code for
;; working with them.

(require 'load-relative)
(require-relative-list '("fringe") "dbgr-")

(defstruct dbgr-loc
"Our own location type. Even though a mark contains a
file-name (via a buffer) and a line number (via an offset), we
want to save the values that were seen/requested originally."
   id          ;; Unique id -- is the total number of locations seen
	       ;; when this one was created.
   filename
   line-number
   marker      ;; Position in source code
   cmd-marker  ;; Position in command process buffer
)

(defalias 'dbgr-loc? 'dbgr-loc-p)

(defun dbgr-loc-current(source-buffer cmd-marker)
  "Create a location object for the point in the current buffer."
  (with-current-buffer source-buffer
    (make-dbgr-loc 
     :filename (buffer-file-name source-buffer)
     :line-number (line-number-at-pos) 
     :marker (point-marker)
     :cmd-marker cmd-marker
    )))

(defun dbgr-loc-marker=(loc marker)
  (setf (dbgr-loc-marker loc) marker))

(defun dbgr-loc-goto(loc)
  "Position a buffer at LOC which may involve reading in a file
and setting the point to the place indicated by LOC. In the
process, the marker inside loc may be updated.

The buffer containing the location referred to, the source-code
buffer, is returned if LOC is found. nil is returned if LOC is
not not found"
  (if (dbgr-loc? loc) 
      (lexical-let* ((filename    (dbgr-loc-filename loc))
		     (line-number (dbgr-loc-line-number loc))
		     (marker      (dbgr-loc-marker loc))
		     (cmd-marker  (dbgr-loc-cmd-marker loc))
		     (src-buffer  (marker-buffer (or marker (make-marker)))))
	(if (not src-buffer)
	    (setq src-buffer (find-file-noselect filename)))
	(if cmd-marker
	    (with-current-buffer (marker-buffer cmd-marker)
	      (goto-char cmd-marker)))
	(if src-buffer
	    (progn 
	      (set-buffer src-buffer)
	      (if (and marker (marker-position marker))
		  (goto-char (marker-position marker))
		(let ((src-marker))
		  (goto-char (point-min))
		  (forward-line (- line-number 1))
		  (setq src-marker (point-marker))
		  (dbgr-loc-marker= loc src-marker)
		  ))))
	src-buffer )))

(provide-me "dbgr-")
