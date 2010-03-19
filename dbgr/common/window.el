(require 'load-relative)
(require-relative-list '("helper" "buffer") "dbgr-")

(fn-p-to-fn?-alias 'one-window-p)
(declare-function one-window?(bool))

(defun dbgr-window-update-position (buffer marker)
  "Update BUFFER to position specified with MARKER.
We assume MAKER points inside BUFFER"
  (with-current-buffer buffer
    (let ((window (get-buffer-window buffer)))
      (if window
	  (progn 
	    (select-window window)
	    (goto-char marker))))))


(defun dbgr-window-src ( &optional opt-buffer )
  "Make sure the source buffer is displayed in a window
We don't care if the command buffer is also displayed.
See also `dbgr-window-src-undistub-cmd'"
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (dbgr-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (window (selected-window)))
    (if src-buffer 
	(unless src-window
	  (set-window-buffer window src-buffer))
	)))

(defun dbgr-window-src-undisturb-cmd ( &optional opt-buffer )
  "Make sure the source buffers is displayed in windows without
disturbing the command window if it is also displayed. Returns
the command window
See also `dbgr-window-src-window'"
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (dbgr-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (dbgr-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 (window (selected-window))
	 )
    (if src-buffer 
	(unless src-window
	  (setq src-window 
		(if (eq window cmd-window)
		    ;; FIXME: generalize what to do here.
		    (if (one-window? 't) 
			(split-window) 
		      (next-window window))
		  window))
	  (set-window-buffer src-window src-buffer))
	)
    cmd-window)
  )

(defun dbgr-window-cmd-undisturb-src ( &optional opt-buffer switch?)
  "Make sure the source buffers is displayed in windows without
disturbing the command window if it is also displayed. Returns
the command window
See also `dbgr-window-src-window'"
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (dbgr-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (dbgr-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 (window (selected-window))
	 )
    (if cmd-buffer 
	(progn 
	  (unless cmd-window
	    (setq cmd-window 
		  (if (eq window src-window)
		      ;; FIXME: generalize what to do here.
		      (if (one-window? 't) 
			  (split-window) 
			(next-window window))
		    window))
	    (set-window-buffer cmd-window cmd-buffer)
	    )
	  (if switch? 
	      (and (select-window cmd-window)
		   (switch-to-buffer cmd-buffer))))

      )
    src-window)
  )

;; (defun dbgr-window-src-and-cmd ( &optional opt-buffer )
;;   "Make sure the source buffers is displayed in windows without
;; disturbing the command window if it is also displayed. Returns
;; the command window
;; See also `dbgr-window-src-window'"
;;   (interactive)
;;   (let* ((buffer (or opt-buffer (current-buffer)))
;; 	 (src-buffer (dbgr-get-srcbuf buffer))
;; 	 (src-window (get-buffer-window src-buffer))
;; 	 (cmd-buffer (dbgr-get-cmdbuf buffer))
;; 	 (cmd-window (get-buffer-window cmd-buffer))
;; 	 (window (selected-window))
;; 	 )
;;     (if src-buffer 
;; 	(unless src-window
;; 	  (setq src-window 
;; 		(if (eq window cmd-window)
;; 		    (if (one-window? 't) (split-window) (next-window window))
;; 		  window))
;; 	  (set-window-buffer src-window src-buffer))
;; 	)
;;     cmd-window)
;;   )

(provide-me "dbgr-")
