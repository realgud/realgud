;; Code associated with breakpoints

(require 'image)
(require 'load-relative)
(require-relative-list '("loc") "dbgr-")

;; NOTE: if you don't see the icon, check the that the window margin
;; is not nil.
(defconst dbgr-bp-xpm-data
  "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint icon.")

(defconst dbr-bp-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

(defconst dbgr-bp-disabled-pbm-data
  "P1
10 10\",
0 0 1 0 1 0 1 0 0 0
0 1 0 1 0 1 0 1 0 0
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
0 0 1 0 1 0 1 0 1 0
0 0 0 1 0 1 0 1 0 0"
  "PBM data used for disabled breakpoint icon.")

(makunbound 'dbgr-bp-enabled-icon)
(defvar dbgr-bp-enabled-icon
  (create-image dbgr-bp-xpm-data
		'xpm t 
		:ascent 100)
  "Icon for an enabled breakpoint in display margin.")

;; For seeing what dbgr-bp-enabled-icon looks like:
;; (insert-image dbgr-bp-enabled-icon)

(makunbound 'dbgr-bp-disabled-icon)
(defvar dbgr-bp-disabled-icon
  (create-image dbgr-bp-disabled-pbm-data
		'pbm t 
		:ascent 100)
  "Icon for a disabled breakpoint in display margin.")

;; For seeing what dbgr-bp-enabled-icon looks like:
;; (insert-image dbgr-bp-disabled-icon)

(defun dbgr-bp-add-info (loc)
  "Record bp information for location LOC."
  (if (dbgr-loc? loc) 
      (let* ((marker (dbgr-loc-marker loc))
	     (bp-num (dbgr-loc-bp-num loc))
	     )
	(dbgr-bp-put-icon marker 't bp-num)
	)
    )
)

(defun dbgr-bp-put-icon (pos enabled bp-num &optional opt-buf)
  "Add a breakpoint icon in the left margin at POS via a `put-image' overlay.
The alternate string name for the image is created from the value
of ENABLED and BP-NUM.  In particular, if ENABLED is 't and
BP-NUM is 5 the overlay string is be 'B5:' If ENABLED is false
then the overlay string is 'b5:'. Breakpoint text properties are
also attached to the icon via its display string."
  (let ((enabled-str)
	(buf (or opt-buf (current-buffer)))
	(bp-num-str
	 (cond 
	  ((or (not bp-num) (not (numberp bp-num))) ":")
	  ('t (format "%d:" bp-num))))
	(brkpt-icon)
	(bp-str)
	(help-string "mouse-1: clear bkpt, mouse-3: enable/disable bkpt")
	)
    (with-current-buffer buf
      (if enabled 
	  (progn 
	    (setq enabled-str "B")
	    (setq brkpt-icon dbgr-bp-enabled-icon)
	    )
	(progn
	  (setq enabled-str "b")
	  (setq brkpt-icon dbgr-bp-disabled-icon)
	  ))
      ;; Create alternate display string and attach
      ;; properties it.
      (setq bp-str (concat enabled-str bp-num-str))
      (add-text-properties
       0 1 `(dbgr-bptno ,bp-num enabled ,enabled) bp-str)
      (add-text-properties
       0 1 (list 'help-echo (format "%s %s" bp-str help-string))
       bp-str)
      
      ;; Display breakpoint icon or display string.  If the window is
      ;; nil, the image doesn't get displayed, so make sure it is large
      ;; enough.
      (let ((window (get-buffer-window (current-buffer) 0)))
	(if window
	    (set-window-margins window 2)
	  ;; FIXME: This is all crap, but I don't know how to fix.
	  (let ((buffer-save (window-buffer (selected-window))))
	    (set-window-buffer (selected-window) (current-buffer))
	    (set-window-margins (selected-window) 2)
	    (set-window-buffer (selected-window) buffer-save))
	  ))
      (dbgr-bp-remove-icons pos)
      (put-image brkpt-icon pos bp-str 'left-margin)
      )
    )
  )

(defun dbgr-bp-remove-icons (&optional opt-begin-pos opt-end-pos)
  "Remove dbgr breakpoint icons (overlays) in the region
OPT-BEGIN-POS to OPT-END-POS. The default value for OPT-BEGIN-POS
is `point'.  The default value for OPT-END-POS is OPT-BEGIN-POS.

The way we determine if an overlay is ours is by inspecting the
overlay for a before-string property containing one we normally set.
"
  (interactive "r")
  (let* ((begin-pos (or opt-begin-pos (point)))
	 (end-pos (or opt-end-pos begin-pos))
	)
    (dolist (overlay (overlays-in begin-pos end-pos))
      ;; We determine if this overlay is one we set by seeing if the 
      ;; string in its 'before-string property has a 'dbgr-bptno property
      (let ((before-string (overlay-get overlay 'before-string)))
	(when (and before-string (get-text-property 0 'dbgr-bptno before-string))
	  (delete-overlay overlay)
	  )
	)
      )
    )
  )

(provide-me "dbgr-")
