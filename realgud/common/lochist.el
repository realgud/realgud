;; Copyright (C) 2010, 2012, 2014-2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Debugger location ring
;;; Commentary:

;; This file manages a ring of (recently stopped) positions to allow
;; the programmer to move between them.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ring)
(require 'org)
(require 'load-relative)
(require-relative-list '("loc") "realgud-")

(declare-function realgud:loc-describe 'realgud-loc)

(defcustom realgud-loc-hist-size 20  ; For testing. Should really be larger.
  "Size of realgud's position history ring"
  :type 'integer
  :group 'realgud)

(cl-defstruct realgud-loc-hist
  "A list of source-code positions recently encountered"
  (position -1)
  (ring (make-ring realgud-loc-hist-size)))

(defun realgud:loc-hist-describe(loc-hist)
  "Format LOC-HIST values inside buffer *Describe Debugger Session*"
  (switch-to-buffer (get-buffer-create "*Describe Debugger Session*"))
  (org-mode)
  (insert "** Source Positions Stopped at (loc-hist)\n")
  (mapc 'insert
	(list
	 (format "  -  buffer size  :: %d\n" realgud-loc-hist-size)
	 (format "  -  position     :: %d\n"
		 (realgud-loc-hist-position loc-hist))))
  (let ((locs (cddr (realgud-loc-hist-ring loc-hist)))
	(loc)
	(i 0))
    (while (and (< i (length locs)) (setq loc (elt locs i)) (realgud-loc? loc) )
      (insert (format "*** %d\n" i))
      (realgud:loc-describe loc)
      (setq i (1+ i))
      )
    )
)

(defun realgud-loc-hist-item-at(loc-hist position)
  "Get the current item stored at POSITION of the ring
component in LOC-HIST"
  (let ((ring (realgud-loc-hist-ring loc-hist)))
    (unless (ring-empty-p ring)
      (ring-ref ring position))))

(defun realgud-loc-hist-item(loc-hist)
  "Get the current item of LOC-HIST at the position previously set"
  (realgud-loc-hist-item-at
   loc-hist
   (realgud-loc-hist-position loc-hist)))

(defun realgud-loc-hist-add(loc-hist loc)
  "Add FRAME to LOC-HIST"
  ;; Switching frames shouldn't save a new ring
  ;; position. Also make sure no position is different.
  ;; Perhaps duplicates should be controlled by an option.
  (let* ((ring (realgud-loc-hist-ring loc-hist))
	 (old-loc (realgud-loc-hist-item loc-hist)))
    (unless (and
	     old-loc
	     (equal (realgud-loc-filename old-loc)
		    (realgud-loc-filename loc))
	     (equal (realgud-loc-line-number old-loc)
		    (realgud-loc-line-number loc))
	     (equal (realgud-loc-column-number old-loc)
		    (realgud-loc-column-number old-loc)))
      (setf (realgud-loc-hist-position loc-hist) 0)
      (ring-insert ring loc)
    )
    ))

(defun realgud-loc-hist-clear(loc-hist)
  "Clear out all source locations in LOC-HIST"
  (let* ((ring (ring-ref (realgud-loc-hist-ring loc-hist)
			 (realgud-loc-hist-position loc-hist)))
	 (head (car ring)))
    (setf (realgud-loc-hist-position loc-hist) (- head 1))
    (while (not (ring-empty-p ring))
      (ring-remove ring))))

(defun realgud-loc-hist-index(loc-hist)
  "Return the ring-index value of LOC-HIST"
  (let* (
	 (ring (realgud-loc-hist-ring loc-hist))
	 (head (car ring))
	 (ringlen (cadr ring))
	 (index (mod (+ ringlen head
			(- (realgud-loc-hist-position loc-hist)))
		     ringlen)))
    (if (zerop index) ringlen index)
    ))

(defun realgud-loc-hist-set (loc-hist position)
  "Set LOC-HIST to POSITION in the stopping history"
  (setf (realgud-loc-hist-position loc-hist) position))

;; FIXME: add numeric arg?
(defun realgud-loc-hist-newer (loc-hist)
  "Set LOC-HIST position to an newer position."

  (setf (realgud-loc-hist-position loc-hist)
	(ring-minus1 (realgud-loc-hist-position loc-hist)
		    (ring-length (realgud-loc-hist-ring loc-hist)))))

(defun realgud-loc-hist-newest (loc-hist)
  "Set LOC-HIST position to the newest position."
  (setf (realgud-loc-hist-position loc-hist) -1))

;; FIXME: add numeric arg?
(defun realgud-loc-hist-older (loc-hist)
  "Set LOC-HIST position to an older position."
    (setf (realgud-loc-hist-position loc-hist)
	 (ring-plus1 (realgud-loc-hist-position loc-hist)
		      (ring-length (realgud-loc-hist-ring loc-hist)))))

(defun realgud-loc-hist-oldest (loc-hist)
  "Set LOC-HIST to the oldest stopping point."
  (let* ((ring (realgud-loc-hist-ring loc-hist))
	 (head (car ring)))
    (setf (realgud-loc-hist-position loc-hist) head)))

(provide-me "realgud-")
