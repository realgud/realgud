; Copyright (C) 2010, 2014, 2019 Free Software Foundation, Inc

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
(require-relative-list '("../fringe" "../helper" "../lochist" "locals")
		       "realgud-")
(require-relative-list '("command" "source" "backtrace" "breakpoint") "realgud-buffer-")

(declare-function realgud-backtrace?        'realgud-buffer-backtace)
(declare-function realgud-breakpoint?       'realgud-buffer-breakpoint)
(declare-function realgud-locals?           'realgud-buffer-locals)
(declare-function realgud-cmdbuf?           'realgud-buffer-command)
(declare-function realgud:loc-hist-describe 'realgud-lochist)
(declare-function realgud-loc-hist-item     'realgud-lochist)
(declare-function realgud-srcbuf?           'realgud-buffer-command)
(declare-function buffer-killed?            'realgud-helper)

(defvar realgud-cmdbuf-info)

(defun realgud-get-cmdbuf-from-backtrace ( &optional opt-buffer)
  "Return the command buffer associated with source
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a source-code buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (realgud-backtrace? buffer)
	(with-current-buffer-safe buffer
	  (realgud-sget 'backtrace-info 'cmdbuf))
      nil)))

(defun realgud-get-cmdbuf-from-breakpoint ( &optional opt-buffer)
  "Return the command buffer associated with source
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a source-code buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (realgud-breakpoint? buffer)
	(with-current-buffer-safe buffer
	  (realgud-sget 'breakpoint-info 'cmdbuf))
      nil)))

(defun realgud-get-cmdbuf-from-locals ( &optional opt-buffer)
  "Return the command buffer associated with source
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a source-code buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (realgud-locals? buffer)
	(with-current-buffer-safe buffer
	  (realgud-sget 'locals-info 'cmdbuf))
      nil)))

(defun realgud-get-cmdbuf-from-srcbuf ( &optional opt-buffer)
  "Return the command buffer associated with source
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a source-code buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (realgud-srcbuf? buffer)
	(with-current-buffer-safe buffer
	  (realgud-sget 'srcbuf-info 'cmdproc))
      nil)))

(defun realgud-get-srcbuf-from-cmdbuf ( &optional opt-buffer opt-loc)
  "Return the source-code buffer associated with command
OPT-BUFFER or if that is ommited `current-buffer' which is
assumed to be a process-command buffer."
  (let ((buffer (or opt-buffer (current-buffer))))
    (if (realgud-cmdbuf? buffer)
	(with-current-buffer-safe buffer
	  (let ((loc
		 (or opt-loc
		     (realgud-loc-hist-item
		      (realgud-cmdbuf-info-loc-hist realgud-cmdbuf-info)))))
	    (if loc
		(marker-buffer (realgud-loc-marker loc))
	      nil)
	    ))
      nil)))

(defun realgud-get-srcbuf( &optional opt-buffer opt-loc)
  "Return source-code buffer associated with OPT-BUFFER or
`current-buffer' if that is omitted. nil is returned if we don't
find anything. If we started out with a buffer that is set up to
be a source-code buffer we will use that even though it might not
be the source code buffer for the frame that the debugger is
using. See also `realgud-get-current-srcbuf'."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer-safe buffer
      (cond
       ;; Perhaps buffer is a source source-code buffer?
       ((realgud-srcbuf? buffer) buffer)
       ;; Perhaps buffer is a process-command buffer.
       ((realgud-cmdbuf? buffer)
	(realgud-get-srcbuf-from-cmdbuf buffer opt-loc))
       (t nil)))))

(defun realgud-get-current-srcbuf( &optional opt-buffer)
  "Return the source-code buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer-safe buffer
      (let ((cmdbuf
	     (cond
	      ((realgud-srcbuf? buffer)
	       (realgud-get-cmdbuf-from-srcbuf buffer))
	      ((realgud-cmdbuf? buffer)
	       buffer)
	      (t nil))))
	(if cmdbuf
	    (realgud-get-srcbuf-from-cmdbuf cmdbuf)
	  nil)))))

(defun realgud-get-cmdbuf( &optional opt-buffer)
  "Return the command buffer associated with OPT-BUFFER
or `current-buffer' if that is omitted. nil is returned
if we don't find anything."

  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer-safe buffer
      (cond
       ;; Perhaps buffer is a process-command buffer?
       ((realgud-cmdbuf? buffer) buffer)
       ;; Perhaps buffer is a source-code buffer?
       ((realgud-srcbuf? buffer)
	(realgud-get-cmdbuf-from-srcbuf buffer))
       ;; Perhaps buffer is a backtrace buffer?
       ((realgud-backtrace? buffer)
	(realgud-get-cmdbuf-from-backtrace buffer))
       ((realgud-breakpoint? buffer)
	(realgud-get-cmdbuf-from-breakpoint buffer))
       ((realgud-locals? buffer)
	(realgud-get-cmdbuf-from-locals buffer))
       (t nil)))))

(defun realgud-get-backtrace-buf( &optional opt-buffer)
  "Return the backtrace buffer associated with
OPT-BUFFER or if that is ommited `current-buffer'."
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (cmdbuf (realgud-get-cmdbuf buffer)))
    (with-current-buffer-safe cmdbuf
      (realgud-sget 'cmdbuf-info 'bt-buf)
      ))
  )

(defun realgud-get-breakpoint-buf( &optional opt-buffer)
  "Return the backtrace buffer associated with
OPT-BUFFER or if that is ommited `current-buffer'."
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (cmdbuf (realgud-get-cmdbuf buffer)))
    (with-current-buffer-safe cmdbuf
      (realgud-sget 'cmdbuf-info 'brkpt-buf)
      ))
  )

(defun realgud-get-locals-buf( &optional opt-buffer)
  "Return the locals buffer associated with
OPT-BUFFER or if that is ommited `current-buffer'."
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (cmdbuf (realgud-get-cmdbuf buffer)))
    (with-current-buffer-safe cmdbuf
      (realgud-sget 'cmdbuf-info 'locals-buf)
      ))
  )

(defun realgud-get-process (&optional opt-buffer)
  "Return the process buffer associated with OPT-BUFFER or
  `current-buffer' if that is omitted. nil is returned if
we don't find anything."
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (cmdbuf (realgud-get-cmdbuf buffer)))
    (if cmdbuf
	(get-buffer-process cmdbuf)
      nil)
    )
)

(defun realgud:srcbuf-info-describe (&optional buffer)
  "Provide descriptive information of the buffer-local variable
`realgud-srcbuf-info', a defstruct. BUFFER if given is the buffer to
use to get the information from.
"
  (interactive "")
  (setq buffer (realgud-get-srcbuf buffer))
  (if buffer
      (with-current-buffer buffer
	(let ((info realgud-srcbuf-info)
	      (srcbuf-name (buffer-name))
	      (a1 realgud-overlay-arrow1)
	      (a2 realgud-overlay-arrow2)
	      (a3 realgud-overlay-arrow3)
	      )
	  (switch-to-buffer (get-buffer-create "*Describe Debugger Session*"))
	  (delete-region (point-min) (point-max))
	  (mapc 'insert
		(list
		 (format "srcbuf-info for %s\n" srcbuf-name)
		 (format "Was previously read only?: %s\n"
			 (realgud-srcbuf-info-was-read-only? info))
		 (format "Command Process buffer: %s\n"
			 (realgud-srcbuf-info-cmdproc info))

		 ;; FIXME This info isn't part of the src info structure.
		 (format "Overlay arrow 1: %s\n" a1)
		 (format "Overlay arrow 2: %s\n" a2)
		 (format "Overlay arrow 3: %s\n" a3)
		 (format "Location history:\n")
		 ))
	  (realgud:loc-hist-describe  (realgud-srcbuf-info-loc-hist info))
	  )
	)
    (message "Buffer %s is not a debugger source buffer; nothing done."
	     (or buffer (current-buffer)))
    )
  )

(provide-me "realgud-buffer-")
