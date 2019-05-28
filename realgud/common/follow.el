;; Copyright (C) 2015, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Follows or goto's something
(require 'load-relative)

(declare-function realgud:cmd-frame  'realgud-cmds)
(declare-function realgud-loc-marker 'realgud-loc)
(declare-function realgud-loc-p      'realgud-loc)

(defun realgud:follow-mark(mark)
  (when (markerp mark)
    (let ((buffer (marker-buffer mark)))
      (set-buffer buffer)
      (set-window-point (display-buffer buffer) mark)
      (goto-char mark)
    )))


(defun realgud:follow(pos)
  (interactive "%d")
  (let* ((mark (get-text-property pos 'mark))
	 (filename (get-text-property pos 'file))
	 (frame-num (get-text-property pos 'frame-num))
	 )
    (cond ((markerp mark) (realgud:follow-mark mark) 't)
	  ((realgud-loc-p mark) (realgud:follow-mark (realgud-loc-marker mark)) 't)
	  ((stringp filename)
	   (find-file-other-window filename))
	  ((numberp frame-num) (realgud:cmd-frame frame-num))
	  ('t (message "No location property found here")))
    ))

(defun realgud:follow-point()
  (interactive)
  (realgud:follow (point)))

(defun realgud:follow-event(event)
  (interactive "e")
  (realgud:follow (posn-point (event-end event))))

(provide-me "realgud-")
