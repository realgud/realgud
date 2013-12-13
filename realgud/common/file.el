;;; Copyright (C) 2010-2011, 2013 Rocky Bernstein <rocky@gnu.org>
; Should realgud-file-loc-from-line be here or elsewhere?
(require 'load-relative)
(require 'compile) ;; for compilation-find-file
(require-relative-list '("helper" "loc") "realgud-")

(defvar realgud-file-remap (make-hash-table :test 'equal)
  "How to remap files we otherwise can't find in the
  filesystem. The hash key is the file string we saw, and the
  value is associated filesystem string presumably in the
  filesystem")

(declare-function buffer-killed? 'helper)
(declare-function compilation-find-file 'compile)

(defun realgud-file-line-count(filename)
  "Return the number of lines in file FILENAME, or nil FILENAME can't be
found"
  (if (file-exists-p filename)
      (let ((file-buffer (find-file-noselect filename)))
	(with-current-buffer-safe file-buffer
	  (line-number-at-pos (point-max))))
    nil))

(defun realgud-file-loc-from-line(filename line-number
					&optional cmd-marker bp-num ignore-file-re)
  "Return a realgud-loc for FILENAME and LINE-NUMBER

CMD-MARKER and BP-NUM get stored in the realgud-loc object. IGNORE-FILE-RE
is a regular expression describing things that aren't expected to be
found. For example many debuggers create a pseudo file name for eval
expressions. For example (eval 1) of Perl <string> of Python.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (unless (file-readable-p filename)
    (if (and ignore-file-re (string-match ignore-file-re filename))
	(message "tracking ignored for psuedo-file %s" filename)
      ; else
      (let ((remapped-filename))
	(if (gethash filename realgud-file-remap)
	    (progn
	      (setq remapped-filename (gethash filename realgud-file-remap))
	      (if (file-exists-p remapped-filename)
		  (setq filename remapped-filename)
		(remhash filename realgud-file-remap)))
	  (progn
	    (setq remapped-filename
		  (buffer-file-name
		   (compilation-find-file (point-marker) filename nil)))
	    (if (and remapped-filename (file-exists-p remapped-filename))
	      (progn
		(puthash filename remapped-filename realgud-file-remap)
		(setq filename remapped-filename)))
	  )))
      ))
  (if (file-readable-p filename)
      (if (integerp line-number)
	  (if (> line-number 0)
	      (lexical-let ((line-count))
		(if (setq line-count (realgud-file-line-count filename))
		    (if (> line-count line-number)
			; And you thought we'd never get around to
			; doing something other than validation?
			(make-realgud-loc
			 :num         bp-num
			 :cmd-marker  cmd-marker
			 :filename    filename
			 :line-number line-number
			 :marker      (make-marker)
			 )
		      (format "File %s has only %d lines. (Line %d requested.)"
			      filename line-count line-number))
		  (format "Problem getting line count for file `%s'" filename)))
	    (format "line number %s should be greater than 0" line-number))
	(format "%s is not an integer" line-number))
    ;; else
    (format "File named `%s' not readable" filename))
  )

(provide-me "realgud-")
