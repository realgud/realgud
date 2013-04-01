;;; Copyright (C) 2010, 2012 Rocky Bernstein <rocky@gnu.org>
;;; Debugger location
;;; Commentary:

;; This describes a debugger location structure and has code for
;; working with them.

(require 'load-relative)
(require 'loc-changes)
(require-relative-list '("fringe") "realgud-")

(defstruct realgud-loc
"Our own location type. Even though a mark contains a
file-name (via a buffer) and a line number (via an offset), we
want to save the values that were seen/requested originally."
   num           ;; If there is a number such as a breakpoint or frame
		 ;; number associated with this location, this is set.
		 ;; nil otherwise.
   filename
   line-number
   column-number ;; Column offset within line
   marker        ;; Position in source code
   cmd-marker    ;; Position in command process buffer
)

(defalias 'realgud-loc? 'realgud-loc-p)

(defun realgud-loc-describe (loc)
  "Display realgud-cmdcbuf-info.
Information is put in an internal buffer called *Describe*."
  (interactive "")
  (switch-to-buffer (get-buffer-create "*Describe*"))
  (mapc 'insert
	(list
	 (format "    num          : %s\n" (realgud-loc-num loc))
	 (format "    filename     : %s\n" (realgud-loc-filename loc))
	 (format "    line number  : %s\n" (realgud-loc-line-number loc))
	 (format "    column number: %s\n" (realgud-loc-column-number loc))
	 (format "    source marker: %s\n" (realgud-loc-marker loc))
	 (format "    cmdbuf marker: %s\n" (realgud-loc-cmd-marker loc))
	 ))
  )

(defun realgud-loc-current(source-buffer cmd-marker)
  "Create a location object for the point in the current buffer."
  (with-current-buffer source-buffer
    (make-realgud-loc
     :filename (buffer-file-name source-buffer)
     :column-number (current-column)
     :line-number (line-number-at-pos)
     :marker (point-marker)
     :cmd-marker cmd-marker
    )))

(defun realgud-loc-marker=(loc marker)
  (setf (realgud-loc-marker loc) marker))

(defun realgud-loc-goto(loc)
  "Position point in the buffer referred to by LOC. This may
involve reading in a file.In the process, the marker inside loc
may be updated.

The buffer containing the location referred to, the source-code
buffer, is returned if LOC is found. nil is returned if LOC is
not not found"
  (if (realgud-loc? loc)
      (let* ((filename    (realgud-loc-filename loc))
	     (line-number (realgud-loc-line-number loc))
	     (column-number (realgud-loc-column-number loc))
	     (marker      (realgud-loc-marker loc))
	     (cmd-marker  (realgud-loc-cmd-marker loc))
	     (src-buffer  (marker-buffer (or marker (make-marker)))))
	(if (and (not src-buffer) filename)
	    (setq src-buffer (find-file-noselect filename)))
	(if cmd-marker
	    (with-current-buffer (marker-buffer cmd-marker)
	      (goto-char cmd-marker)))
	(if src-buffer
	    (with-current-buffer src-buffer
	      (if (and marker (marker-position marker))
		  ;; A marker has been set in loc, so use that.
		  (goto-char (marker-position marker))
		;; We don't have a position set in the source buffer
		;; so find it and go there. We use `loc-changes-goto'
		;; to find that spot. `loc-changes-goto' keeps a
		;; record of the first time we went to that spot, so
		;; in the face of buffer modifications, it may be more
		;; reliable.
		(let ((src-marker))
		  (loc-changes-goto line-number)
		  (setq src-marker (point-marker))
		  (realgud-loc-marker= loc src-marker)
		  ))))
	src-buffer )))

(provide-me "realgud-")
