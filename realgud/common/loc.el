;; Copyright (C) 2015-2017, 2019 Free Software Foundation, Inc

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

;; This describes a debugger location structure and has code for
;; working with them.

(eval-when-compile (require 'cl-lib))

(require 'load-relative)
(require 'loc-changes)
(require-relative-list '("fringe" "follow") "realgud-")
(require-relative-list '("buffer/source") "realgud-buffer-")

;; FIXME: removed because of recursive loads
;; (require-relative-list '("buffer/helper") "realgud-buffer-")

(declare-function realgud:strip                  'realgud)
(declare-function realgud-get-cmdbuf-from-srcbuf 'realgud-buffer-helper)
(declare-function realgud-srcbuf?                'realgud-buffer-source)

(cl-defstruct realgud-loc
"Our own location type. Even though a mark contains a
file-name (via a buffer) and a line number (via an offset), we
want to save the values that were seen/requested originally."
   num           ;; If there is a number such as a breakpoint or frame
		 ;; number associated with this location, this is set.
		 ;; nil otherwise.
   filename
   line-number
   column-number ;; Column offset within line
   source-text   ;; Source text if available
   marker        ;; Position in source code
   cmd-marker    ;; Position in command process buffer
)

(defalias 'realgud-loc? 'realgud-loc-p)

;; The below function is generic and might be found in standard
;; library. Or it might be moved someplace more generic.
(defun realgud:buffer-line-no-props()
  "Returns a string containing the line that `point' is at,
without buffer properties."
  (buffer-substring-no-properties (point-at-bol)
				  (point-at-eol)))

(defun realgud:loc-describe (loc)
  "Display realgud-cmdcbuf-info.
Information is put in an internal buffer called *Describe Debugger Session*."
  (interactive "")
  (switch-to-buffer (get-buffer-create "*Describe Debugger Session*"))
  (realgud:org-mode-append-loc loc))

(defun realgud:org-mode-append-loc (loc)
  "Display realgud-cmdcbuf-info.
Information is put in an internal buffer called *Describe Debugger Session*."
  (let ((column-number (realgud-loc-column-number loc))
	(bp-num (realgud-loc-num loc))
	(source-text (realgud-loc-source-text loc))
	(filename (realgud-loc-filename loc)))
    (insert "  - filename      :: ")
    (put-text-property
     (insert-text-button filename
			 'action 'realgud:follow-event
			 'help-echo "mouse-2: go to this file")
     (point)
     'file filename)
    (insert "\n")
    (mapc 'insert
	  (list
	   (format "  - line number   :: %s\n" (realgud-loc-line-number loc))
	   (if bp-num
	       (format "  - brkpt num     :: %s\n" (realgud-loc-num loc))
	     "")
	   (if column-number
	       (format "  - column number :: %s\n"
		       (realgud-loc-column-number loc))
	     "")
	   (if source-text
	       (format "  - source text   :: %s\n" (realgud-loc-source-text loc))
	     "")
	   ))
    ;; Make locations clickable
    (insert "  - source marker :: ")
    (put-text-property
     (insert-text-button (format "%s" (realgud-loc-marker loc))
			 'action 'realgud:follow-event
			 'help-echo "mouse-2: go to this source location")
     (point)
     'mark (realgud-loc-marker loc))

    (insert "\n  - cmdbuf marker :: ")
    (put-text-property
     (insert-text-button (format "%s" (realgud-loc-cmd-marker loc))
			 'action 'realgud:follow-event
			 'help-echo "mouse-2: go to this command-buffer location")
     (point)
     'mark (realgud-loc-cmd-marker loc))
    (insert "\n")
    )
  )


(defun realgud-loc-current(&optional source-buffer cmd-marker)
  "Create a location object for the point in the current buffer.
   If SOURCE-BUFFER is not given, take the current buffer as the
   source buffer."
  (interactive "")
  (unless source-buffer
    (setq source-buffer (current-buffer)))
  (unless (realgud-srcbuf? source-buffer)
    (error "%s is not a realgud source buffer" source-buffer))
  (unless cmd-marker
    (setq cmd-marker
	  (realgud-get-cmdbuf-from-srcbuf source-buffer))
    )
  (with-current-buffer source-buffer
    (let ((mark (point-marker))
	  (text (realgud:buffer-line-no-props)))
      (make-realgud-loc
       :filename (buffer-file-name source-buffer)
       :column-number (current-column)
       :line-number (line-number-at-pos)
       :source-text text
       :marker      mark
       :cmd-marker cmd-marker
       )
      )))

(defun realgud-loc-marker=(loc marker)
  (setf (realgud-loc-marker loc) marker))

(defun realgud-loc-goto(loc)
  "Position point in the buffer referred to by LOC. This may
involve reading in a file. In the process, the marker inside LOC
may be updated.

If LOC is found, The buffer containing the location referred to,
the source-code buffer, is returned. Otherwise, nil is returned."
  (if (realgud-loc? loc)
      (let* ((filename    (realgud-loc-filename loc))
	     (line-number (realgud-loc-line-number loc))
	     (column-number (realgud-loc-column-number loc))
	     (marker      (realgud-loc-marker loc))
	     (cmd-marker  (realgud-loc-cmd-marker loc))
	     (use-marker  nil)
	     (src-buffer  (marker-buffer (or marker (make-marker)))))
	(if (and (not src-buffer) filename)
	    (setq src-buffer (find-file-noselect filename)))
	(if cmd-marker
	    (with-current-buffer (marker-buffer cmd-marker)
	      (goto-char cmd-marker)))
	(if src-buffer
	    (with-current-buffer src-buffer
	      (when (and marker (marker-position marker))
		;; A marker has been set in loc, so use that.
		(goto-char (marker-position marker))
		(setq use-marker 't)
		(let ((current-text (realgud:buffer-line-no-props))
		      (loc-text (realgud-loc-source-text loc)))
		  (unless (and loc-text
			       (equal (realgud:strip current-text) (realgud:strip loc-text)))
		    (loc-changes-goto line-number)
		    (setq current-text (realgud:buffer-line-no-props))
		    (when (equal current-text loc-text)
		      (message "Marked location needed updating")
		      (setq use-marker nil))
		    )))
	      (if use-marker
		  (goto-char (marker-position marker))
		;; else
		;; We don't have a position set in the source buffer
		;; so find it and go there. We use `loc-changes-goto'
		;; to find that spot. `loc-changes-goto' keeps a
		;; record of the first time we went to that spot, so
		;; in the face of buffer modifications, it may be more
		;; reliable.
		(let ((src-marker))
		  (loc-changes-goto line-number)
		  (when column-number
		    (move-to-column column-number))
		  (setq src-marker (point-marker))
		  (realgud-loc-marker= loc src-marker)
		  ))))
	src-buffer )))

(provide-me "realgud-")
