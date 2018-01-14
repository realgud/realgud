;; Copyright (C) 2010, 2012-2015, 2017-2018 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; Code associated with breakpoints

(require 'image)
(require 'load-relative)
(require-relative-list '("loc" "bp-image-data") "realgud-")

(defun realgud-bp-remove-icons (&optional begin-pos end-pos bpnum)
  "Remove breakpoint icons (overlays) in BEGIN-POS .. END-POS.
The default value for BEGIN-POS is `point'.  The default value
for END-POS is BEGIN-POS.  When BPNUM is non-nil, only remove
overlays with that breakpoint number.

The way we determine if an overlay is ours is by inspecting the
overlay for a realgud property."
  (interactive "r")
  (setq begin-pos (or begin-pos (point))
        end-pos (or end-pos begin-pos))
  (dolist (overlay (overlays-in begin-pos end-pos))
    (when (overlay-get overlay 'realgud)
      (when (or (null bpnum) (equal bpnum (overlay-get overlay 'realgud-bp-num)))
        (delete-overlay overlay)))))

(defvar realgud-bp-enabled-icon nil
  "Icon for an enabled breakpoint in display margin.")

(defvar realgud-bp-disabled-icon nil
  "Icon for a disabled breakpoint in display margin.")

(defun realgud-set-bp-icons()
  "Load breakpoint icons, if needed."
  (when (display-images-p)
    (unless realgud-bp-enabled-icon
      (setq realgud-bp-enabled-icon
            (find-image `((:type xpm :data
                                 ,realgud-bp-xpm-data
                                 :ascent 100 :pointer hand)
                          (:type svg :data
                                 ,realgud-bp-enabled-svg-data
                                 :ascent 100 :pointer hand)
                          (:type tiff :data
                                 ,realgud-bp-enabled-tiff-data
                                 :ascent 100 :pointer hand)
                          (:type pbm :data
                                 ,realgud-bp-enabled-pbm-data
                                 :ascent 100 :pointer hand)))))
    (unless realgud-bp-disabled-icon
      (setq realgud-bp-disabled-icon
            (find-image `((:type xpm :data
                                 ,realgud-bp-xpm-data
                                 :conversion disabled ; different than 'enabled'
                                 :ascent 100 :pointer hand)
                          (:type svg :data
                                 ,realgud-bp-disabled-svg-data
                                 :ascent 100 :pointer hand)
                          (:type tiff :data
                                 ,realgud-bp-disabled-tiff-data
                                 :ascent 100 :pointer hand)
                          (:type pbm :data
                                 ,realgud-bp-disabled-pbm-data
                                 :ascent 100 :pointer hand)
                          (:type svg :data
                                 ,realgud-bp-disabled-svg-data
                                 :ascent 100 :pointer hand)))))))

(declare-function define-fringe-bitmap "fringe.c"
                  (bitmap bits &optional height width align))

(when (display-images-p)
  ;; Taken from gdb-mi
  (define-fringe-bitmap 'realgud-bp-filled
    "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
  (define-fringe-bitmap 'realgud-bp-hollow
    "\x3c\x42\x81\x81\x81\x81\x42\x3c"))

(defgroup realgud-bp nil
  "RealGUD breakpoints UI"
  :group 'realgud
  :prefix 'realgud-bp-)

(defface realgud-bp-enabled-face
  '((t :foreground "red" :weight bold))
  "Face for enabled breakpoints (in the fringe or margin)."
  :group 'realgud-bp)

(defface realgud-bp-disabled-face
  '((t :foreground "grey" :weight bold))
  "Face for disabled breakpoints (in the fringe or margin).
Only used in text terminals: fringe icons always use
`realgud-bp-enabled-face'."
  :group 'realgud-bp)

(defface realgud-bp-line-enabled-face
  '((t (:box (:color "red"))))
  "Face for lines with enabled breakpoints."
  :group 'realgud-bp)

(defface realgud-bp-line-disabled-face
  '((t (:box (:color "grey"))))
  "Face for lines with disabled breakpoints."
  :group 'realgud-bp)

(defcustom realgud-bp-fringe-indicator-style '(filled-rectangle . hollow-rectangle)
  "Which fringe icon to use for breakpoints."
  :type '(choice (const :tag "Disc" (realgud-bp-filled . realgud-bp-hollow))
                 (const :tag "Square" (filled-square . hollow-square))
                 (const :tag "Rectangle" (filled-rectangle . hollow-rectangle)))
  :group 'realgud-bp)

(defcustom realgud-bp-use-fringe t
  "Whether to use the fringe to display breakpoints.
If nil, use margins instead."
  :type '(boolean)
  :group 'realgud-bp)

(defun realgud-bp--fringe-width ()
  "Compute width of left fringe."
  (let ((window (get-buffer-window (current-buffer))))
    (or left-fringe-width
        (and window (car (window-fringes window)))
        0)))

(defun realgud-bp-add-fringe-icon (overlay icon)
  "Add a fringe icon to OVERLAY.
ICON is a symbol registered with `define-fringe-bitmap'."
  ;; Ensure that the fringe is wide enough
  (unless (>= (realgud-bp--fringe-width) 8)
    (set-fringe-mode `(8 . ,right-fringe-width)))
  ;; Add the fringe icon
  (let* ((fringe-spec `(left-fringe ,icon realgud-bp-enabled-face)))
    (overlay-put overlay 'before-string (propertize "x" 'display fringe-spec))))

(defun realgud-bp-add-margin-indicator (overlay text image face)
  "Add a margin breakpoint indicator to OVERLAY.
TEXT is a string, IMAGE an IMAGE spec or nil; TEXT gets
highlighted with FACE."
  ;; Ensure that the margin is large enough (Taken from gdb-mi)
  (when (< left-margin-width 2)
    (save-current-buffer
      (setq left-margin-width 2)
      (let ((window (get-buffer-window (current-buffer) 0)))
        (if window
            (set-window-margins
             window left-margin-width right-margin-width)))))
  ;; Add the margin string
  (let* ((indicator (or image (propertize text 'face face)))
         (spec `((margin left-margin) ,indicator)))
    (overlay-put overlay 'before-string (propertize text 'display spec))))

(defun realgud-bp-put-icon (pos enable? bp-num &optional buf)
  "Add a breakpoint icon at POS according to breakpoint-display-style.
Use the fringe if available, and the margin otherwise.  Record
breakpoint status ENABLE? and breakpoint number BP-NUM in
overlay.  BUF is the buffer that POS refers to; it defaults to
the current buffer."
  (let* ((bp-text) (bp-face) (line-face) (margin-icon) (fringe-icon))
    (realgud-set-bp-icons)
    (if enable?
        (setq bp-text "B"
              bp-face 'realgud-bp-enabled-face
              line-face 'realgud-bp-line-enabled-face
              margin-icon realgud-bp-enabled-icon
              fringe-icon (car realgud-bp-fringe-indicator-style))
      (setq bp-text "b"
            bp-face 'realgud-bp-disabled-face
            line-face 'realgud-bp-line-disabled-face
            margin-icon realgud-bp-disabled-icon
            fringe-icon (cdr realgud-bp-fringe-indicator-style)))
    (let ((help-echo (format "%s%s: mouse-1 to clear" bp-text bp-num)))
      (setq bp-text (propertize bp-text 'help-echo help-echo)))
    (with-current-buffer (or buf (current-buffer))
      (realgud-bp-remove-icons pos (1+ pos) bp-num)
      (let* ((eol (save-excursion (goto-char pos) (point-at-eol)))
             (ov (make-overlay pos (1+ eol) (current-buffer) t nil)))
        (if (and realgud-bp-use-fringe (display-images-p))
            (realgud-bp-add-fringe-icon ov fringe-icon)
          (realgud-bp-add-margin-indicator ov bp-text margin-icon bp-face))
        (overlay-put ov 'face line-face)
        (overlay-put ov 'realgud t)
        (overlay-put ov 'realgud-bp-num bp-num)
        (overlay-put ov 'realgud-bp-enabled enable?)))))

(defun realgud-bp-del-icon (pos &optional buf bpnum)
  "Delete breakpoint icon at POS.
BUF is the buffer which pos refers to (default: current buffer).
If BPNUM is non-nil, only remove overlays maching that breakpoint
number."
  (with-current-buffer (or buf (current-buffer))
    (realgud-bp-remove-icons pos (1+ pos) bpnum)))

(defun realgud-bp-add-info (loc)
  "Record bp information for location LOC."
  (if (realgud-loc? loc)
      (let* ((marker (realgud-loc-marker loc))
             (bp-num (realgud-loc-num loc)))
        (realgud-bp-put-icon marker t bp-num))))

(defun realgud-bp-del-info (loc)
  "Remove bp information for location LOC."
  (if (realgud-loc? loc)
      (let* ((marker (realgud-loc-marker loc))
             (bp-num (realgud-loc-num loc)))
        (realgud-bp-del-icon marker (current-buffer) bp-num))))

(defun realgud-bp-enable-disable-info (bp-num enable? loc buf)
  "Enable or disable bp with BP-NUM at location LOC in BUF."
  (if (realgud-loc? loc)
      (let* ((marker (realgud-loc-marker loc))
             (bp-num-check (realgud-loc-num loc)))
	(if (eq bp-num bp-num-check)
	    (realgud-bp-put-icon marker enable? bp-num buf)
	  (message "Internal error - bp number found %s doesn't match requested %s"
		   bp-num-check bp-num)
	  ))))

(provide-me "realgud-")
