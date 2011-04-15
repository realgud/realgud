;;; Backtrace buffer
;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../key") "dbgr-")

(require-relative-list
 '("command") "dbgr-buffer-")

(declare-function dbgr-backtrace-mode (cmdbuf))
(declare-function dbgr-cmd-backtrace arg)


(defstruct dbgr-backtrace-info
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  (cmdbuf    nil)  ;; buffer of the associated debugger process
  (cur-pos   0)    ;; Frame we are at
  frame-ring       ;; ring of marks in buffer of frame numbers. The 
                   ;; text at that marker has additional properties about the
                   ;; frame
)

(declare-function dbgr-cmd-frame(num))
(declare-function dbgr-get-cmdbuf(&optional opt-buffer))
(declare-function dbgr-command (fmt &optional arg no-record? 
				    frame-switch? dbgr-prompts?))

(defvar dbgr-backtrace-info)
(make-variable-buffer-local 'dbgr-backtrace-info)

;: FIXME: not picked up from track. Why?
(defvar dbgr-track-divert-string nil)

(defvar dbgr-goto-entry-acc "")

(defun dbgr-get-buffer-base-name(string)
  "Leading and ending * in string. For example:
   *shell<2>* -> shell<2>
   *foo shell* -> foo
   buffer.c -> buffer.c"
  (if (string-match "^[*]?\\([^*]+\\)[*]?$" string)
      (let ((string-sans-stars (match-string 1 string)))
	(if (string-match "\\(.+\\) shell" string-sans-stars)
	    (match-string 1 string-sans-stars)
	  string-sans-stars)
	)
    string
    )
)

;; FIXME: create this in a new frame.
(defun dbgr-backtrace-init ()
  (interactive)
  (let ((buffer (current-buffer))
  	(cmdbuf (dbgr-get-cmdbuf))
  	(process)
  	)
    (with-current-buffer-safe cmdbuf
      (let ((frame-pat (dbgr-cmdbuf-pat "debugger-backtrace"))
	    (indicator-re (or (dbgr-cmdbuf-pat "selected-frame-indicator")
			      "->"))
	    (selected-frame-num)
	    (frame-pos-ring)
	    (sleep-count 0)
	    )
	(unless frame-pat
	  (error "No 'debugger-backtrace' regular expression recorded for debugger %s"
		 (dbgr-cmdbuf-debugger-name)))
	(setq process (get-buffer-process (current-buffer)))
	(dbgr-cmdbuf-info-in-srcbuf?= (not (dbgr-cmdbuf? buffer)))
	(dbgr-cmdbuf-info-divert-output?= 't)
	(setq dbgr-track-divert-string nil)
	(dbgr-cmd-backtrace 0)
	(while (and (eq 'run (process-status process))
		    (null dbgr-track-divert-string)
		    (> 1000 (setq sleep-count (1+ sleep-count))))
	  (sleep-for 0.001)
	  )
	(if (>= sleep-count 1000)
	    (message "Timeout on running debugger command")
	  ;; else
	  ;; (message "+++4 %s" dbgr-track-divert-string)
	  (let ((bt-buffer (get-buffer-create
			    (format "*%s backtrace*" 
				    (dbgr-get-buffer-base-name
				     (buffer-name)))))
		(divert-string dbgr-track-divert-string)
		)
	    (dbgr-cmdbuf-info-bt-buf= bt-buffer)
	    (with-current-buffer bt-buffer
	      (setq buffer-read-only nil)
	      (delete-region (point-min) (point-max))
	      (if divert-string 
		  (let* ((triple 
			  (dbgr-backtrace-add-text-properties frame-pat
							      divert-string
							      indicator-re))
			 (string-with-props (car triple))
			 (frame-num-pos-list (caddr triple))
			 )
		    (setq selected-frame-num (cadr triple))
		    (insert string-with-props)
		    ;; add marks for each position
		    (dbgr-backtrace-mode cmdbuf)
		    (setq frame-pos-ring 
			  (make-ring (length frame-num-pos-list)))
		    (dolist (pos frame-num-pos-list)
		      (goto-char (1+ pos))
		      (ring-insert-at-beginning frame-pos-ring (point-marker))
		      )
		    )
		)
	      ;; dbgr-backtrace-mode kills all local variables so
	      ;; we set this after. Alternatively change dbgr-backtrace-mode.
	      (set (make-local-variable 'dbgr-backtrace-info)
		   (make-dbgr-backtrace-info
		    :cmdbuf cmdbuf
		    :frame-ring frame-pos-ring
		    ))
	      (if selected-frame-num
		  (dbgr-backtrace-moveto-frame selected-frame-num))
	      )
	    )
	  )
	)
      )
    (unless cmdbuf
      (message "Unable to find debugger command buffer for %s" buffer))
    )
  )

(defun dbgr-backtrace? ( &optional buffer)
  "Return true if BUFFER is a debugger command buffer."
  (with-current-buffer-safe 
   (or buffer (current-buffer))
   (dbgr-backtrace-info-set?)))


(defalias 'dbgr-backtrace-info? 'dbgr-backtrace-info-p)

(defun dbgr-backtrace-info-set? ()
  "Return true if dbgr-backtrace-info is set."
  (and (boundp 'dbgr-backtrace-info) 
       dbgr-backtrace-info
       (dbgr-backtrace-info? dbgr-backtrace-info)))


(defun dbgr-backtrace-moveto-frame-selected ()
  "Set point to the selected frame."
  (interactive)
  (if (dbgr-backtrace?)
      (let* ((cur-pos (dbgr-sget 'backtrace-info 'cur-pos))
	     (ring-size (ring-size (dbgr-sget 'backtrace-info 'frame-ring)))
	     )
	(if (and cur-pos (> ring-size 0))
	    (dbgr-backtrace-moveto-frame cur-pos)
	  ;else
	  (message "No frame information recorded")
	  )
	)
    )
  )

(defun dbgr-backtrace-moveto-frame (num &optional opt-buffer)
  (if (integerp num)
      (if (dbgr-backtrace?)
	  (let* ((ring (dbgr-sget 'backtrace-info 'frame-ring))
		 (marker (ring-ref ring num)))
	    (setf (dbgr-backtrace-info-cur-pos dbgr-backtrace-info) num)
	    (goto-char marker)
	    )
	)
    ; else
    (message "frame number %s is not an integer" num)
    )
  )

(defun dbgr-backtrace-moveto-frame-next ()
  "Set point to the next frame. If we are at the end, wrap to the
beginning. Note that we are just moving in the backtrace buffer,
not updating the frame stack."
  (interactive)
  (if (dbgr-backtrace?)
      (let* ((cur-pos (dbgr-sget 'backtrace-info 'cur-pos))
	     (ring-size (ring-size (dbgr-sget 'backtrace-info 'frame-ring)))
	     )
	(if (and cur-pos (> ring-size 0))
	    (dbgr-backtrace-moveto-frame (ring-plus1 cur-pos ring-size))
	  ;else
	  (message "No frame information recorded")
	  )
	)
    )
  )

(defun dbgr-backtrace-moveto-frame-prev ()
  "Set point to the next frame. If we are at the beginning, wrap to the
end. Note that we are just moving in the backtrace buffer,
not updating the frame stack."
  (interactive)
  (if (dbgr-backtrace?)
      (let* ((cur-pos (dbgr-sget 'backtrace-info 'cur-pos))
	     (ring-size (ring-size (dbgr-sget 'backtrace-info 'frame-ring)))
	     )
	(if (and cur-pos (> ring-size 0))
	    (dbgr-backtrace-moveto-frame (ring-minus1 cur-pos ring-size))
	  ;else
	  (message "No frame information recorded")
	  )
      )
    )
  )

(defun dbgr-goto-frame-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq dbgr-goto-entry-acc (concat dbgr-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc dbgr-goto-entry-acc))
          (while (not (string= acc ""))
            (if (not (dbgr-goto-entry-try acc))
                (setq acc (substring acc 1))
              (dbgr-cmd-frame (string-to-number acc))
              ;; Break loop.
              (setq acc "")))))
    (message "`dbgr-goto-frame-n' must be bound to a number key")))

;; FIXME: replace with ring.
(defun dbgr-goto-entry-try (str)
  "See if there is an entry with number STR.  If not return nil."
  (goto-char (point-min))
  (if (re-search-forward (concat "^[^0-9]*\\(" str "\\)[^0-9]") nil t)
      (progn
        (goto-char (match-end 1))
        t)
    nil))


;; The following is split in two to facilitate debugging.
(defun dbgr-goto-entry-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq dbgr-goto-entry-acc (concat dbgr-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc dbgr-goto-entry-acc)
              (p (point)))
          (while (not (string= acc ""))
            (if (not (dbgr-goto-entry-try acc))
                (setq acc (substring acc 1))
              (setq p (point))
              ;; Break loop.
              (setq acc "")))
          (goto-char p)))
    (message "`dbgr-goto-entry-n' must be bound to a number key")))


(defun dbgr-goto-entry-n ()
  "Go to an entry number.

Breakpoints, Display expressions and Stack Frames all have
numbers associated with them which are distinct from line
numbers.  In a secondary buffer, this function is usually bound to
a numeric key which will position you at that entry number.  To
go to an entry above 9, just keep entering the number.  For
example, if you press 1 and then 9, you should jump to entry
1 (if it exists) and then 19 (if that exists).  Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'dbgr-goto-entry-n))
      (setq dbgr-goto-entry-acc ""))
  (dbgr-goto-entry-n-internal (this-command-keys)))

(defun dbgr-goto-frame ()
  "Go to the frame number. We get the frame number from the
'frame-num property"
  (interactive)
  (if (dbgr-backtrace?)
      (let ((frame-num (get-text-property (point) 'frame-num)))
	(if frame-num 
	    (dbgr-cmd-frame frame-num)
	  (message "No frame property found at this point")
	  )
	)
    )
  )

(defun dbgr-goto-frame-n ()
  "Go to the frame number indicated by the accumulated numeric keys just entered.

This function is usually bound to a numeric key in a 'frame'
secondary buffer. To go to an entry above 9, just keep entering
the number. For example, if you press 1 and then 9, frame 1 is selected
\(if it exists) and then frame 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'dbgr-goto-frame-n))
      (setq dbgr-goto-entry-acc ""))
  (dbgr-goto-frame-n-internal (this-command-keys)))

(defun dbgr-backtrace-add-text-properties  (frame-pat &optional opt-string
						      frame-indicator-re)
  "Parse STRING and add properties for that"

  (let ((string (or opt-string 
		    (buffer-substring (point-min) (point-max))
		    ))
	(frame-regexp (dbgr-loc-pat-regexp frame-pat))
	(frame-group-pat (dbgr-loc-pat-num frame-pat))
	(alt-frame-num -1)
	(last-pos 0)
	(selected-frame-num nil)
	(frame-num-pos-list '())
	)
    (while (string-match frame-regexp string last-pos)
      (let ((frame-num-str)
	    (frame-num)
	    
	    ;; FIXME: Remove hack that 1 is always the frame indicator.
	    (frame-indicator 
	     (substring string (match-beginning 1) (match-end 1)))
	    (frame-num-pos)

	    )
	(if frame-group-pat
	    (progn
	      (setq frame-num-str 
		    (substring string (match-beginning frame-group-pat)
			       (match-end frame-group-pat)))
	      (setq frame-num (string-to-number frame-num-str))
	      (setq frame-num-pos (match-beginning frame-group-pat))
	      (add-to-list 'frame-num-pos-list frame-num-pos 't)
	      (add-text-properties (match-beginning frame-group-pat) 
				   (match-end frame-group-pat)
				   '(mouse-face highlight 
						help-echo 
						"mouse-2: goto this frame")
				   string)
	      
	      )
	  ; else
	  (progn 
	    (setq frame-num-str 
		    (substring string (match-beginning 0)
			       (match-end 0)))
	    (setq frame-num (incf alt-frame-num))
	    (setq frame-num-pos (match-beginning 0))
	    (add-to-list 'frame-num-pos-list frame-num-pos 't)
	    (add-text-properties (match-beginning 0) 
				 (match-end 0)
				 '(mouse-face highlight 
					      help-echo 
					      "mouse-2: goto this frame")
				   string)
	    )
	  )

	
	(put-text-property (match-beginning 0) (match-end 0)
			   'frame-num  frame-num string)
	(setq last-pos (match-end 0))

	(if (string-match frame-indicator-re frame-indicator)
	  (setq selected-frame-num frame-num))
	))

    (list string selected-frame-num frame-num-pos-list)
    )
  )

(provide-me "dbgr-buffer-")
