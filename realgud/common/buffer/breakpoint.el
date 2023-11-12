;;; Breakpoint buffer

;; Author: Rocky Bernstein <rocky@gnu.org>

;; Copyright (C) 2019 Free Software Foundation, Inc

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(require 'ansi-color)
(require 'ring)
(require 'seq)
(require 'load-relative)
(eval-when-compile (require 'cl-lib))
(require-relative-list
 '("../key" "helper" "../follow" "../loc") "realgud-")

(require-relative-list
 '("command") "realgud-buffer-")

(declare-function realgud-breakpoint-mode              'realgud-breakpoint-mod)
(declare-function realgud-get-buffer-base-name        'realgud-buffer-backtrace)
(declare-function realgud-cmdbuf-debugger-name        'realgud-buffer-command)
(declare-function realgud-cmdbuf?                     'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-bkpt-buf=       'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-divert-output?= 'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-in-srcbuf?=     'realgud-buffer-command)
(declare-function realgud:cmd-breakpoint              'realgud-cmds)
(declare-function realgud:cmd-info-breakpoints 'realgud-cmds)
(declare-function realgud-cmdbuf-pat                  'realgud-buffer-command)
(declare-function realgud-get-cmdbuf                  'realgud-buffer-helper)
(declare-function realgud:file-loc-from-line          'realgud-file)
(declare-function realgud-loc-goto                    'realgud-loc)
(declare-function buffer-killed?       'realgud-helper)
(declare-function realgud:loc-describe 'realgud-loc)

(cl-defstruct realgud-breakpoint-info
  "debugger object/structure specific to a (top-level) program to be debugged."
  (cmdbuf    nil)  ;; buffer of the associated debugger process
  (cur-pos   0)    ;; beakpoint we are at
  breakpoint-ring  ;; ring of marks in buffer of breakpoint numbers. The
                   ;; text at that marker has additional properties about the
                   ;; breakpoint
)

(declare-function realgud:cmd-frame 'realgud-buffer-command)
(declare-function realgud-get-cmdbuf(&optional opt-buffer))
(declare-function realgud-command 'realgud-send)

(make-variable-buffer-local (defvar realgud-breakpoint-info))

;: FIXME: not picked up from track. Why?
(defvar realgud-track-divert-string nil)

(defvar realgud-goto-entry-acc "")

(defun realgud:breakpoint-describe (&optional buffer)
  (interactive "")
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((breakpoints (realgud-breakpoint-info-breakpoint-ring realgud-breakpoint-info))
	  (brkpt)
	  (loc)
	  (i 0))
      (switch-to-buffer (get-buffer-create "*Describe Breakpoints*"))
      (while (and (< i (ring-length breakpoints)) (setq brkpt (ring-ref breakpoints i)))
	(insert (format "*** %d\n" i))
	(insert (format "%s\n" brkpt))
	(when (markerp brkpt)
	  (with-current-buffer (marker-buffer brkpt)
	    (goto-char brkpt)
	    (setq loc (get-text-property (point) 'loc))
	  )
	  (when loc (realgud:loc-describe loc)))
	(setq i (1+ i))
      )
    )
    ))

;; FIXME: create this in a new frame.
(defun realgud:breakpoint-init ()
  (interactive)
  (let ((buffer (current-buffer))
  	(cmdbuf (realgud-get-cmdbuf))
  	(process)
  	)
    (with-current-buffer-safe cmdbuf
      (let ((brkpt-pat (realgud-cmdbuf-pat "debugger-breakpoint"))
	    (brkpt-pos-ring)
	    (bp-list (realgud-cmdbuf-info-bp-list realgud-cmdbuf-info))
	    (sleep-count 0)
	    )
	(unless brkpt-pat
	  (error "No 'debugger-breakpoint' regular expression recorded for debugger %s"
		 (realgud-cmdbuf-debugger-name)))
	(setq process (get-buffer-process (current-buffer)))
	(realgud-cmdbuf-info-in-srcbuf?= (not (realgud-cmdbuf? buffer)))
	(realgud-cmdbuf-info-divert-output?= t)
	(setq realgud-track-divert-string nil)
	(realgud:cmd-info-breakpoints)
	(while (and (eq 'run (process-status process))
		    (null realgud-track-divert-string)
		    (> 1000 (setq sleep-count (1+ sleep-count))))
	  (sleep-for 0.001)
	  )
	(if (>= sleep-count 1000)
	    (message "Timeout on running debugger command")
	  ;; else
	  ;; (message "+++4 %s" realgud-track-divert-string)
	  (let ((brkpt-buffer (get-buffer-create
			    (format "*Breakpoint %s*"
				    (realgud-get-buffer-base-name
				     (buffer-name)))))
		(divert-string realgud-track-divert-string)
		)
	    (realgud-cmdbuf-info-brkpt-buf= brkpt-buffer)
	    (with-current-buffer brkpt-buffer
	      (setq buffer-read-only nil)
	      (delete-region (point-min) (point-max))
	      (if divert-string
		  (let* ((duple
			  (realgud:breakpoint-add-text-properties
			   brkpt-pat cmdbuf divert-string bp-list))
			 (string-with-props
			  (ansi-color-filter-apply (car duple)))
			 (brkpt-num-pos-list (cadr duple))
			 )
		    (insert string-with-props)
		    ;; add marks for each position
		    (realgud-breakpoint-mode cmdbuf)
		    (setq brkpt-pos-ring
			  (make-ring (length brkpt-num-pos-list)))
		    (dolist (pos brkpt-num-pos-list)
		      (goto-char (1+ pos))
		      (ring-insert-at-beginning brkpt-pos-ring (point-marker))
		      )
		    )
		)
	      ;; realgud-breakpoint-mode kills all local variables so
	      ;; we set this after. Alternatively change realgud-breakpoint-mode.
	      (set (make-local-variable 'realgud-breakpoint-info)
		   (make-realgud-breakpoint-info
		    :cmdbuf cmdbuf
		    :breakpoint-ring brkpt-pos-ring
		    ))
	      )
	    )
	  )
	)
      )
    (unless cmdbuf
      (message "Unable to find debugger command buffer for %s" buffer))
    )
  )

(defun realgud-breakpoint? ( &optional buffer)
  "Return true if BUFFER is a debugger command buffer."
  (with-current-buffer-safe
   (or buffer (current-buffer))
   (realgud-breakpoint-info-set?)))


(defalias 'realgud-breakpoint-info? 'realgud-breakpoint-info-p)

(defun realgud-breakpoint-info-set? ()
  "Return true if realgud-breakpoint-info is set."
  (and (boundp 'realgud-breakpoint-info)
       realgud-breakpoint-info
       (realgud-breakpoint-info? realgud-breakpoint-info)))


(defun realgud-goto-entry-n ()
  "Go to an entry number.

Breakpoints, Display expressions and Stack Frames all have
numbers associated with them which are distinct from line
numbers.  In a secondary buffer, this function is usually bound to
a numeric key which will position you at that entry number.  To
go to an entry above 9, just keep entering the number.  For
example, if you press 1 and then 9, you should jump to entry
1 (if it exists) and then 19 (if that exists).  Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'realgud-goto-entry-n))
      (setq realgud-goto-entry-acc ""))
  (realgud-goto-entry-n-internal (this-command-keys)))

(defun realgud-goto-breakpoint ()
  "Go to the breakpoint number. We get the breakpoint number from the
'brkpt-num property"
  (interactive)
  (if (realgud-breakpoint?)
      (let ((loc (get-text-property (point) 'loc)))
	(if loc
	    (realgud-loc-goto loc)
	  (message "No location property found at this point")
	  )
	)
    )
  )

(defun realgud-goto-breakpoint-mouse (event)
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
	 (loc (get-text-property pos 'loc)))
    (if (realgud-breakpoint?)
	(if loc
	    (realgud-loc-goto loc)
	  (message "No location property found at this point")
	  )
      )
    )
)

(defun realgud-goto-breakpoint-n ()
  "Goto breakpoint number indicated by the accumulated numeric keys just entered.

This function is usually bound to a numeric key in a 'frame'
secondary buffer. To go to an entry above 9, just keep entering
the number. For example, if you press 1 and then 9, frame 1 is selected
\(if it exists) and then frame 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'realgud-goto-breakpoint-n))
      (setq realgud-goto-entry-acc ""))
  (realgud-goto-breakpoint-n-internal (this-command-keys)))

(defun realgud-goto-breakpoint-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq realgud-goto-entry-acc (concat realgud-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc realgud-goto-entry-acc))
          (while (not (string= acc ""))
            (if (not (realgud-goto-entry-try acc))
                (setq acc (substring acc 1))
              (realgud:cmd-frame (string-to-number acc))
              ;; Break loop.
              (setq acc "")))))
    (message "`realgud-goto-breakpoint-n' must be bound to a number key")))

(defun realgud:breakpoint-add-text-properties(brkpt-pat cmdbuf string bp-list)
  "Parse STRING or the current buffer and add frame properties: breakpoint number,
filename, and line number as text properties."

  (let* ((stripped-string (ansi-color-filter-apply string))
	 (brkpt-regexp (realgud-loc-pat-regexp brkpt-pat))
	 (brkpt-group-pat (realgud-loc-pat-num brkpt-pat))
	 (file-group-pat (realgud-loc-pat-file-group brkpt-pat))
	 (line-group-pat (realgud-loc-pat-line-group brkpt-pat))
	 (alt-brkpt-num -1)
	 (last-pos 0)
	 (selected-brkpt-num nil)
	 (brkpt-num-pos-list '())
	 )
    (while (string-match brkpt-regexp stripped-string last-pos)
      (let ((brkpt-num-str) (brkpt-num) (line-num) (filename)
	    (loc)
	    ;; From https://github.com/realgud/realgud/pull/192
	    ;; Each brkpt of breakpoint is searched via string-match
	    ;; invocation and a position of the current brkpt is
	    ;; updated via (setq last-pos (match-end 0)) in the end of
	    ;; the loop. But somewhere in the body of the loop (I do
	    ;; not know exactly where), there is another call to
	    ;; string-match and it messes up all positions.
	    (whole-match-begin (match-beginning 0))
	    (whole-match-end (match-end 0))
	    (brkpt-num-pos)

	    )
	(if brkpt-group-pat
	    (progn
	      (setq brkpt-num-str
		    (substring stripped-string
			       (match-beginning brkpt-group-pat)
			       (match-end brkpt-group-pat)))
	      (setq brkpt-num (string-to-number brkpt-num-str))
	      (setq loc (seq-find (lambda (elt) (equal brkpt-num (realgud-loc-num elt))) bp-list))
	      (setq brkpt-num-pos (match-beginning brkpt-group-pat))
	      (cl-pushnew brkpt-num-pos brkpt-num-pos-list)
	      (when loc
		(add-text-properties (match-beginning brkpt-group-pat)
				     (match-end brkpt-group-pat)
				     (list 'mouse-face 'highlight
					   'help-echo "mouse-2: goto this brkpt"
					   'mark (realgud-loc-marker loc))
				     string))
	      )
	  ; else
	  (progn
	    (setq brkpt-num-str
		    (substring stripped-string (match-beginning 0)
			       (match-end 0)))
	    (setq brkpt-num (cl-incf alt-brkpt-num))
	    (setq brkpt-num-pos (match-beginning 0))
	    (cl-pushnew brkpt-num-pos brkpt-num-pos-list)
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'mouse-face 'highlight
				       'help-echo "mouse-2: goto this brkpt"
				       'brkpt brkpt-num)
				 string)
	    )
	  )
	(when file-group-pat
	  (setq filename (substring stripped-string
				    (match-beginning file-group-pat)
				    (match-end file-group-pat)))
	  (add-text-properties (match-beginning file-group-pat)
			       (match-end file-group-pat)
			       (list 'mouse-face 'highlight
				     'help-echo "mouse-2: goto this file"
				     'action 'realgud:follow-event
				     'file filename)
			       string)
	    )
	(when line-group-pat
	  (let ((line-num-str (substring stripped-string
				    (match-beginning line-group-pat)
				    (match-end line-group-pat))))
	    (setq line-num (string-to-number (or line-num-str "1")))
	  ))

	(when (and (stringp filename) (numberp line-num))
	  (let ((loc (realgud:file-loc-from-line filename line-num cmdbuf)))
	    (put-text-property whole-match-begin whole-match-end
			       'mark loc string)
	    ))
	(put-text-property whole-match-begin whole-match-end
			   'brkpt-num  brkpt-num string)
	(setq last-pos whole-match-end)
	))

    (list string (nreverse brkpt-num-pos-list))
    )
  )

(provide-me "realgud-buffer-")
