;; Copyright (C) 2010, 2014-2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(require 'load-relative)
(require-relative-list '("helper") "realgud-")
(require-relative-list '("buffer/helper" "buffer/locals") "realgud-buffer-")

(declare-function realgud:backtrace-init    'realgud-buffer-helper)
(declare-function realgud:breakpoint-init   'realgud-buffer-helper)
(declare-function realgud-get-backtrace-buf 'realgud-buffer-helper)
(declare-function realgud-get-breakpoint-buf 'realgud-buffer-helper)
(declare-function realgud-get-cmdbuf        'realgud-buffer-helper)
(declare-function realgud-get-srcbuf        'realgud-buffer-helper)
(declare-function buffer-killed?            'realgud-helper)

(declare-function one-window-p(bool))

(defcustom realgud-window-split-orientation 'vertical
  "Orientation when splitting window."
  :type '(choice (const :tag "Vertical" vertical)
                 (const :tag "Horizontal" horizontal))
  :group 'realgud)

(defun realgud-prepare-window (window)
  "Setup layout based on WINDOW, then select and return a window."
  (if (one-window-p 't)
      (cond
       ((eq realgud-window-split-orientation 'vertical)
        (split-window-vertically))
       ((eq realgud-window-split-orientation 'horizontal)
        (split-window-horizontally))
       (t (error "TODO: implement customized layout")))
    (next-window window 'no-minibuf)))

(defun realgud-window-update-position (buffer marker)
  "Update BUFFER to position specified with MARKER.
We assume MARKER points inside BUFFER"
  (with-current-buffer buffer
    (goto-char marker)
    (let ((window (get-buffer-window buffer)))
      (if window (set-window-point window marker))
      )))


(defun realgud-window-src ( &optional opt-buffer )
  "Make sure the source buffer is displayed in a window
We don't care if the command buffer is also displayed.
See also `realgud-window-src-undisturb-cmd'"
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (realgud-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer 'visible))
	 (window (selected-window)))
    (if src-buffer
	(unless (and src-window (not (window-minibuffer-p)))
	  (set-window-buffer window src-buffer))
	)
    ))

(defun realgud-window-src-undisturb-cmd ( &optional opt-buffer )
  "Make sure the source buffers is displayed in windows without
disturbing the command window if it is also displayed. Returns
the command window
See also `realgud-window-src'."
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (realgud-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (realgud-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 )
    (if src-buffer
	(unless src-window
	  (setq src-window (realgud-prepare-window (selected-window)))
	  (set-window-buffer src-window src-buffer))
	)
    (select-window src-window)
    cmd-window)
  )

(defun realgud-window-cmd-undisturb-src ( &optional opt-buffer switch?)
  "Make sure the command buffer is displayed in windows without
disturbing the command window if it is also displayed. Returns
the source window."
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (realgud-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (realgud-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 )
    (when cmd-buffer
      (unless cmd-window
	(setq cmd-window (realgud-prepare-window (selected-window)))
	(set-window-buffer cmd-window cmd-buffer)
	)
      (if switch?
	  (and (select-window cmd-window)
	       (switch-to-buffer cmd-buffer)))

      )
    (select-window cmd-window)
    src-window)
  )

(defun realgud:window-bt-undisturb-src ( &optional opt-buffer switch?)
  "Make sure the backtrace buffer is displayed in windows without
disturbing the source window if it is also displayed. Returns
the source window
See also `realgud-window-src'"
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (realgud-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (realgud-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 (bt-buffer (realgud-get-backtrace-buf cmd-buffer))
	 (bt-window (get-buffer-window bt-buffer))
	 )
    (when cmd-buffer
      (unless bt-window
	(setq bt-window (realgud-prepare-window (selected-window)))
	(set-window-buffer bt-window bt-buffer)
	)
      (if switch?
	  (and (select-window bt-window)
	       (switch-to-buffer bt-buffer)))

      )
    src-window)
  )

(defun realgud:window-brkpt-undisturb-src ( &optional opt-buffer switch?)
  "Make sure the backtrace buffer is displayed in windows without
disturbing the source window if it is also displayed. Returns
the source window
See also `realgud-window-src'"
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (realgud-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (realgud-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 (brkpt-buffer (realgud-get-breakpoint-buf cmd-buffer))
	 (brkpt-window (get-buffer-window brkpt-buffer))
	 )
    (when cmd-buffer
      (unless brkpt-window
	(setq brkpt-window (realgud-prepare-window (selected-window)))
	(set-window-buffer brkpt-window brkpt-buffer)
	)
      (if switch?
	  (and (select-window brkpt-window)
	       (switch-to-buffer brkpt-buffer)))

      )
    src-window)
  )

(defun realgud:window-locals-undisturb-src ( &optional opt-buffer switch?)
  "Make sure the locals buffer is displayed in windows without
disturbing the source window if it is also displayed. Returns
the source window
See also `realgud-window-src'"
  (interactive)
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (src-buffer (realgud-get-srcbuf buffer))
	 (src-window (get-buffer-window src-buffer))
	 (cmd-buffer (realgud-get-cmdbuf buffer))
	 (cmd-window (get-buffer-window cmd-buffer))
	 (locals-buffer (realgud-get-locals-buf cmd-buffer))
	 (locals-window (get-buffer-window locals-buffer))
	 )
    (when cmd-buffer
      (unless locals-window
	(setq locals-window (realgud-prepare-window (selected-window)))
	(set-window-buffer locals-window locals-buffer)
	)
      (if switch?
	  (and (select-window locals-window)
	       (switch-to-buffer locals-buffer)))

      )
    src-window)
  )


(defun realgud:window-bt()
  "Refresh backtrace information and display that in a buffer"
  (interactive)
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (realgud:backtrace-init)
    (realgud:window-bt-undisturb-src)
    )
  )


(defun realgud:window-brkpt()
  "Refresh breakpoint information and display that in a buffer"
  (interactive)
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (realgud:breakpoint-init)
    (realgud:window-brkpt-undisturb-src)
    )
  )


(defun realgud:window-locals()
  "Refresh locals information and display that in a buffer"
  (interactive)
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (realgud-locals-init)
    (realgud:window-locals-undisturb-src)
    )
  )

;; (defun realgud-window-src-and-cmd ( &optional opt-buffer )
;;   "Make sure the source buffers is displayed in windows without
;; disturbing the command window if it is also displayed. Returns
;; the command window
;; See also `realgud-window-src-window'"
;;   (interactive)
;;   (let* ((buffer (or opt-buffer (current-buffer)))
;; 	 (src-buffer (realgud-get-srcbuf buffer))
;; 	 (src-window (get-buffer-window src-buffer))
;; 	 (cmd-buffer (realgud-get-cmdbuf buffer))
;; 	 (cmd-window (get-buffer-window cmd-buffer))
;; 	 (window (selected-window))
;; 	 )
;;     (if src-buffer
;; 	(unless src-window
;; 	  (setq src-window
;; 		(if (eq window cmd-window)
;; 		    (if (one-window-p 't) (split-window) (next-window window))
;; 		  window))
;; 	  (set-window-buffer src-window src-buffer))
;; 	)
;;     cmd-window)
;;   )

(provide-me "realgud-")
