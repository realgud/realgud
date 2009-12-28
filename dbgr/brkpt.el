;; Code associated with breakpoints

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

(defun dbgr-add-bp-info (loc)
  "Record bp information for location LOC."
)

(defun dbgr-put-bp-icon (pos enabled &optional bp-num)
  "Add a breakpoint icon in the left margin at POS via a `put-image' overlay.
The alternate string name for the image is created from the value
of ENABLED and BP-NUM.  In particular, if ENABLED is 't and
BP-NUM is 5 the overlay string is be 'B5:' If ENABLED is false then the 
overlay string is 'b5:'."
  (let ((enabled-str)
	(bp-num-str
	 (cond 
	  ((or (not bp-num) (not (numberp bp-num))) ":")
	  ('t (format "%d:" bp-num))))
	(brkpt-icon)
	(bp-str)
	)
    (if enabled 
	(progn 
	  (setq enabled-str "B")
	  (setq brkpt-icon dbgr-bp-enabled-icon)
	  )
      (progn
	(setq enabled-str "b")
	(setq brkpt-icon dbgr-bp-disabled-icon)
	))
    (setq bp-str (concat enabled-str bp-num-str))
    (unless (window-margins)
      (set-window-margins (selected-window) 1))
    (put-image brkpt-icon pos bp-str 'left-margin)
    )
  )

(defun dbgr-remove-bp-icons (&optional opt-begin-pos opt-end-pos)
  "Remove a breakpoint information regarding location LOC."
  (interactive)
  (let* ((begin-pos (or opt-begin-pos (point)))
	 (end-pos (or opt-end-pos begin-pos))
	)
    (dolist (overlay (overlays-in begin-pos end-pos))
      ;;(when (overlay-get overlay 'put-dbgr-break)
	  (delete-overlay overlay)
	  ;;)
      )
    )
  )
