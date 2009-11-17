;; Bitmap for breakpoint in fringe
(define-fringe-bitmap 'breakpoint
  "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
;; Bitmap for gud-overlay-arrow in fringe
(define-fringe-bitmap 'hollow-right-triangle
  "\xe0\x90\x88\x84\x84\x88\x90\xe0")

(defun dbgr-set-arrow (marker &optional opt-indicator-pair)
  "Set the fringe indicator or overlay arrow to MARKER. This is done
for example to indicate a debugger position."
  (save-excursion
    (let ((indicator-pair 
	   (or opt-indicator-pair
	   '((overlay-arrow . right-triangle)))))
      (set-buffer (marker-buffer marker))
      (setq overlay-arrow-position marker)
      (setq overlay-arrow-string "=>")
      (setq fringe-indicator-alist indicator-pair))))

(defun dbgr-unset-arrow (&optional buffer)
  "Remove fringe indicator or overlay arrow for BUFFER."
  (save-excursion
      (set-buffer (or buffer (current-buffer)))
      (setq overlay-arrow-position nil)))

(provide 'dbgr-arrow)
