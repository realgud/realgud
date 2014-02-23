;;; Backtrace buffer
;;; Copyright (C) 2010-2014 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(eval-when-compile (require 'cl-lib))
(require-relative-list
 '("../key" "helper") "realgud-")

(require-relative-list
 '("command") "realgud-buffer-")

(declare-function realgud-backtrace-mode (cmdbuf))
(declare-function realgud-cmd-backtrace (arg))
(declare-function realgud-cmdbuf-pat(key))
(declare-function realgud-cmdbuf-info-in-srcbuf?= (arg))
(declare-function realgud-get-cmdbuf 'realgud-buffer-helper)

(defstruct realgud-backtrace-info
  "debugger object/structure specific to a (top-level) program to be debugged."
  (cmdbuf    nil)  ;; buffer of the associated debugger process
  (cur-pos   0)    ;; Frame we are at
  frame-ring       ;; ring of marks in buffer of frame numbers. The
                   ;; text at that marker has additional properties about the
                   ;; frame
)

(declare-function realgud-cmd-frame(num))
(declare-function realgud-get-cmdbuf(&optional opt-buffer))
(declare-function realgud-command (fmt &optional arg no-record?
				    frame-switch? realgud-prompts?))

(make-variable-buffer-local (defvar realgud-backtrace-info))

;: FIXME: not picked up from track. Why?
(defvar realgud-track-divert-string nil)

(defvar realgud-goto-entry-acc "")

(defun realgud-get-buffer-base-name(string)
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
(defun realgud-backtrace-init ()
  (interactive)
  (let ((buffer (current-buffer))
  	(cmdbuf (realgud-get-cmdbuf))
  	(process)
  	)
    (with-current-buffer-safe cmdbuf
      (let ((frame-pat (realgud-cmdbuf-pat "debugger-backtrace"))
	    (indicator-re (or (realgud-cmdbuf-pat "selected-frame-indicator")
			      "->"))
	    (selected-frame-num)
	    (frame-pos-ring)
	    (sleep-count 0)
	    )
	(unless frame-pat
	  (error "No 'debugger-backtrace' regular expression recorded for debugger %s"
		 (realgud-cmdbuf-debugger-name)))
	(setq process (get-buffer-process (current-buffer)))
	(realgud-cmdbuf-info-in-srcbuf?= (not (realgud-cmdbuf? buffer)))
	(realgud-cmdbuf-info-divert-output?= 't)
	(setq realgud-track-divert-string nil)
	(realgud-cmd-backtrace 0)
	(while (and (eq 'run (process-status process))
		    (null realgud-track-divert-string)
		    (> 1000 (setq sleep-count (1+ sleep-count))))
	  (sleep-for 0.001)
	  )
	(if (>= sleep-count 1000)
	    (message "Timeout on running debugger command")
	  ;; else
	  ;; (message "+++4 %s" realgud-track-divert-string)
	  (let ((bt-buffer (get-buffer-create
			    (format "*%s backtrace*"
				    (realgud-get-buffer-base-name
				     (buffer-name)))))
		(divert-string realgud-track-divert-string)
		)
	    (realgud-cmdbuf-info-bt-buf= bt-buffer)
	    (with-current-buffer bt-buffer
	      (setq buffer-read-only nil)
	      (delete-region (point-min) (point-max))
	      (if divert-string
		  (let* ((triple
			  (realgud-backtrace-add-text-properties frame-pat
							      divert-string
							      indicator-re))
			 (string-with-props (car triple))
			 (frame-num-pos-list (caddr triple))
			 )
		    (setq selected-frame-num (cadr triple))
		    (insert string-with-props)
		    ;; add marks for each position
		    (realgud-backtrace-mode cmdbuf)
		    (setq frame-pos-ring
			  (make-ring (length frame-num-pos-list)))
		    (dolist (pos frame-num-pos-list)
		      (goto-char (1+ pos))
		      (ring-insert-at-beginning frame-pos-ring (point-marker))
		      )
		    )
		)
	      ;; realgud-backtrace-mode kills all local variables so
	      ;; we set this after. Alternatively change realgud-backtrace-mode.
	      (set (make-local-variable 'realgud-backtrace-info)
		   (make-realgud-backtrace-info
		    :cmdbuf cmdbuf
		    :frame-ring frame-pos-ring
		    ))
	      (if selected-frame-num
		  (realgud-backtrace-moveto-frame selected-frame-num))
	      )
	    )
	  )
	)
      )
    (unless cmdbuf
      (message "Unable to find debugger command buffer for %s" buffer))
    )
  )

(defun realgud-backtrace? ( &optional buffer)
  "Return true if BUFFER is a debugger command buffer."
  (with-current-buffer-safe
   (or buffer (current-buffer))
   (realgud-backtrace-info-set?)))


(defalias 'realgud-backtrace-info? 'realgud-backtrace-info-p)

(defun realgud-backtrace-info-set? ()
  "Return true if realgud-backtrace-info is set."
  (and (boundp 'realgud-backtrace-info)
       realgud-backtrace-info
       (realgud-backtrace-info? realgud-backtrace-info)))


(defun realgud-backtrace-moveto-frame-selected ()
  "Set point to the selected frame."
  (interactive)
  (if (realgud-backtrace?)
      (let* ((cur-pos (realgud-sget 'backtrace-info 'cur-pos))
	     (ring-size (ring-size (realgud-sget 'backtrace-info 'frame-ring)))
	     )
	(if (and cur-pos (> ring-size 0))
	    (realgud-backtrace-moveto-frame cur-pos)
	  ;else
	  (message "No frame information recorded")
	  )
	)
    )
  )

(defun realgud-backtrace-moveto-frame (num &optional opt-buffer)
  (if (integerp num)
      (if (realgud-backtrace?)
	  (let* ((ring (realgud-sget 'backtrace-info 'frame-ring))
		 (marker (ring-ref ring num)))
	    (setf (realgud-backtrace-info-cur-pos realgud-backtrace-info) num)
	    (goto-char marker)
	    )
	)
    ; else
    (message "frame number %s is not an integer" num)
    )
  )

(defun realgud-backtrace-moveto-frame-next ()
  "Set point to the next frame. If we are at the end, wrap to the
beginning. Note that we are just moving in the backtrace buffer,
not updating the frame stack."
  (interactive)
  (if (realgud-backtrace?)
      (let* ((cur-pos (realgud-sget 'backtrace-info 'cur-pos))
	     (ring-size (ring-size (realgud-sget 'backtrace-info 'frame-ring)))
	     )
	(if (and cur-pos (> ring-size 0))
	    (realgud-backtrace-moveto-frame (ring-plus1 cur-pos ring-size))
	  ;else
	  (message "No frame information recorded")
	  )
	)
    )
  )

(defun realgud-backtrace-moveto-frame-prev ()
  "Set point to the next frame. If we are at the beginning, wrap to the
end. Note that we are just moving in the backtrace buffer,
not updating the frame stack."
  (interactive)
  (if (realgud-backtrace?)
      (let* ((cur-pos (realgud-sget 'backtrace-info 'cur-pos))
	     (ring-size (ring-size (realgud-sget 'backtrace-info 'frame-ring)))
	     )
	(if (and cur-pos (> ring-size 0))
	    (realgud-backtrace-moveto-frame (ring-minus1 cur-pos ring-size))
	  ;else
	  (message "No frame information recorded")
	  )
      )
    )
  )

(defun realgud-goto-frame-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq realgud-goto-entry-acc (concat realgud-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc realgud-goto-entry-acc))
          (while (not (string= acc ""))
            (if (not (realgud-goto-entry-try acc))
                (setq acc (substring acc 1))
              (realgud-cmd-frame (string-to-number acc))
              ;; Break loop.
              (setq acc "")))))
    (message "`realgud-goto-frame-n' must be bound to a number key")))

;; FIXME: replace with ring.
(defun realgud-goto-entry-try (str)
  "See if there is an entry with number STR.  If not return nil."
  (goto-char (point-min))
  (if (re-search-forward (concat "^[^0-9]*\\(" str "\\)[^0-9]") nil t)
      (progn
        (goto-char (match-end 1))
        t)
    nil))


;; The following is split in two to facilitate debugging.
(defun realgud-goto-entry-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq realgud-goto-entry-acc (concat realgud-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc realgud-goto-entry-acc)
              (p (point)))
          (while (not (string= acc ""))
            (if (not (realgud-goto-entry-try acc))
                (setq acc (substring acc 1))
              (setq p (point))
              ;; Break loop.
              (setq acc "")))
          (goto-char p)))
    (message "`realgud-goto-entry-n' must be bound to a number key")))


(defun realgud-goto-entry-n ()
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
  (if (not (eq last-command 'realgud-goto-entry-n))
      (setq realgud-goto-entry-acc ""))
  (realgud-goto-entry-n-internal (this-command-keys)))

(defun realgud-goto-frame ()
  "Go to the frame number. We get the frame number from the
'frame-num property"
  (interactive)
  (if (realgud-backtrace?)
      (let ((frame-num (get-text-property (point) 'frame-num)))
	(if frame-num
	    (realgud-cmd-frame frame-num)
	  (message "No frame property found at this point")
	  )
	)
    )
  )

(defun realgud-goto-frame-n ()
  "Go to the frame number indicated by the accumulated numeric keys just entered.

This function is usually bound to a numeric key in a 'frame'
secondary buffer. To go to an entry above 9, just keep entering
the number. For example, if you press 1 and then 9, frame 1 is selected
\(if it exists) and then frame 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'realgud-goto-frame-n))
      (setq realgud-goto-entry-acc ""))
  (realgud-goto-frame-n-internal (this-command-keys)))

(defun realgud-backtrace-add-text-properties  (frame-pat &optional opt-string
						      frame-indicator-re)
  "Parse STRING and add properties for that"

  (let ((string (or opt-string
		    (buffer-substring (point-min) (point-max))
		    ))
	(frame-regexp (realgud-loc-pat-regexp frame-pat))
	(frame-group-pat (realgud-loc-pat-num frame-pat))
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

(provide-me "realgud-buffer-")
