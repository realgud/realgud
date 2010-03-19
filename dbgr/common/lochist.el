;;; Debugger location ring
;;; Commentary:

;; This file manages a ring of (recently stopped) positions to allow
;; the programmer to move between them.

;;; Code:
 
(require 'ring)
(require 'load-relative)
(require-relative-list '("loc") "dbgr-")

(defcustom dbgr-loc-hist-size 20  ; For testing. Should really be larger.
  "Size of dbgr position history ring"
  :type 'integer
  :group 'dbgr)

(defstruct dbgr-loc-hist
  "A list of source-code positions recently encountered"
  (position -1)
  (ring (make-ring dbgr-loc-hist-size)))
  
(defun dbgr-loc-hist-item-at(loc-hist position)
  "Get the current item stored at POSITION of the ring
component in LOC-HIST"
  (lexical-let ((ring (dbgr-loc-hist-ring loc-hist)))
    (if (ring-empty-p ring)
	nil
      (ring-ref ring position))))

(defun dbgr-loc-hist-item(loc-hist)
  "Get the current item of LOC-HIST at the position previously set"
  (dbgr-loc-hist-item-at 
   loc-hist
   (dbgr-loc-hist-position loc-hist)))

(defun dbgr-loc-hist-add(loc-hist item)
  "Add FRAME to LOC-HIST"
  ;; Switching frames shouldn't save a new ring
  ;; position. Also make sure no position is different.
  ;; Perhaps duplicates should be controlled by an option.
  (let* ((ring (dbgr-loc-hist-ring loc-hist)))
    ;;(unless (equal (dbgr-loc-hist-item loc-hist) item)
      (setf (dbgr-loc-hist-position loc-hist) 0)
      (ring-insert ring item)
    ;;) 
    ))

(defun dbgr-loc-hist-clear(loc-hist)
  "Clear out all source locations in LOC-HIST"
  (lexical-let* ((ring (ring-ref (dbgr-loc-hist-ring loc-hist)
				 (dbgr-loc-hist-position loc-hist)))
		 (head (car ring)))
    (setf (dbgr-loc-hist-position loc-hist) (- head 1))
    (while (not (ring-empty-p ring))
      (ring-remove ring))))

(defun dbgr-loc-hist-index(loc-hist)
  "Return the ring-index value of LOC-HIST"
  (lexical-let* (
		 (ring (dbgr-loc-hist-ring loc-hist))
		 (head (car ring))
		 (ringlen (cadr ring))
		 (index (mod (+ ringlen head 
				(- (dbgr-loc-hist-position loc-hist)))
			     ringlen)))
    (if (zerop index) ringlen index)
    ))

(defun dbgr-loc-hist-set (loc-hist position)
  "Set LOC-HIST to POSITION in the stopping history"
  (setf (dbgr-loc-hist-position loc-hist) position))

;; FIXME: add numeric arg? 
(defun dbgr-loc-hist-newer (loc-hist)
  "Set LOC-HIST position to an newer position."
  
  (setf (dbgr-loc-hist-position loc-hist) 
	(ring-minus1 (dbgr-loc-hist-position loc-hist)
		    (ring-length (dbgr-loc-hist-ring loc-hist)))))

(defun dbgr-loc-hist-newest (loc-hist)
  "Set LOC-HIST position to the newest position."
  (setf (dbgr-loc-hist-position loc-hist) -1))
  
;; FIXME: add numeric arg? 
(defun dbgr-loc-hist-older (loc-hist)
  "Set LOC-HIST position to an older position."
    (setf (dbgr-loc-hist-position loc-hist) 
	 (ring-plus1 (dbgr-loc-hist-position loc-hist)
		      (ring-length (dbgr-loc-hist-ring loc-hist)))))

(defun dbgr-loc-hist-oldest (loc-hist)
  "Set LOC-HIST to the oldest stopping point."
  (lexical-let* ((ring (dbgr-loc-hist-ring loc-hist))
		 (head (car ring)))
    (setf (dbgr-loc-hist-position loc-hist) head)))

(provide-me "dbgr-")

