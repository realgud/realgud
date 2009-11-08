;; Bitmap for breakpoint in fringe
(define-fringe-bitmap 'breakpoint
  "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
;; Bitmap for gud-overlay-arrow in fringe
(define-fringe-bitmap 'hollow-right-triangle
  "\xe0\x90\x88\x84\x84\x88\x90\xe0")

(defun dbgr-set-arrow (mark)
  "Set the fringe arrow in MARKER to indicate the top frame."
  (save-excursion
    (let ((indicator-pair 
	   '((overlay-arrow . right-triangle))))
      (switch-to-buffer (marker-buffer mark))
      (setq overlay-arrow-position mark)
      (setq overlay-arrow-string "=>")
      (setq fringe-indicator-alist indicator-pair))))

(defun dbgr-unset-arrow (buffer)
  "Set the fringe arrow in MARKER to indicate the top frame."
  (save-excursion
      (switch-to-buffer buffer)
      (setq overlay-arrow-position nil)))




