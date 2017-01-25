;; Copyright (C) 2010-2011, 2013-2014, 2016-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

; Should realgud:file-loc-from-line be here or elsewhere?
(require 'load-relative)
(require 'compile) ;; for compilation-find-file
(require-relative-list '("helper" "loc") "realgud-")

(defvar realgud-file-remap (make-hash-table :test 'equal)
  "How to remap files we otherwise can't find in the
  filesystem. The hash key is the file string we saw, and the
  value is associated filesystem string presumably in the
  filesystem")


(declare-function realgud:strip         'realgud)
(declare-function realgud-loc-goto      'realgud-loc)
(declare-function buffer-killed?        'helper)
(declare-function compilation-find-file 'compile)

(defcustom realgud-file-find-function 'compilation-find-file
  "Function to call when we can't easily find file"
  :type 'function
  :group 'realgud)

(defun realgud:file-line-count(filename)
  "Return the number of lines in file FILENAME, or nil FILENAME can't be
found"
  (if (file-exists-p filename)
      (let ((file-buffer (find-file-noselect filename)))
	(with-current-buffer-safe file-buffer
	  (line-number-at-pos (point-max))))
    nil))

(defun realgud:file-column-from-string(filename line-number source-text
						&optional no-strip-blanks)
  "Return the column of the first column position of SOURCE-TEXT
at LINE-NUMBER or nil if it is not there"
  (condition-case nil
      (when (and source-text (file-exists-p filename))
        (let ((file-buffer (find-file-noselect filename)))
          (with-current-buffer-safe file-buffer
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line-number))
              (unless no-strip-blanks
                (setq source-text (realgud:strip source-text)))
              (when (search-forward source-text (point-at-eol))
                (goto-char (match-beginning 0))
                (current-column))))))
    (error nil)))

;; FIXME: should allow column number to be passed in.
(defun realgud:file-loc-from-line(filename line-number
					   &optional cmd-marker source-text bp-num
					   ;; FIXME: remove ignore-file-re and cover with
					   ;; find-file-fn.
					   ignore-file-re find-file-fn directory)
  "Return a realgud-loc for FILENAME and LINE-NUMBER and the
other optional position information.

CMD-MARKER and BP-NUM get stored in the realgud-loc
object. FIND-FILE-FN is a function which do special things to
transform filename so it can be found. This could include
searching classpaths (in Java), stripping leading and trailing
blanks, or deliberately ignoring 'pseudo-file patterns like (eval
1) of Perl and <string> of Python.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (unless (and filename (file-readable-p filename))
    (if find-file-fn
        (setq filename (funcall find-file-fn filename))
      ;; FIXME: Remove the below by refactoring to use the above find-file-fn
      ;; else
      (if (and ignore-file-re (string-match ignore-file-re filename))
          (message "tracking ignored for pseudo-file %s" filename)
        ;; else
        (let ((remapped-filename))
          (if (gethash filename realgud-file-remap)
              (progn
                (setq remapped-filename (gethash filename realgud-file-remap))
                (if (file-exists-p remapped-filename)
                    (setq filename remapped-filename)
                  (remhash filename realgud-file-remap)))
            ;; else
            (let ((found-file (funcall realgud-file-find-function (point-marker) filename directory)))
                (when found-file
                  (setq remapped-filename (buffer-file-name found-file))
                  (when (and remapped-filename (file-exists-p remapped-filename))
                    (puthash filename remapped-filename realgud-file-remap)
                    (setq filename remapped-filename)
                    ))
                )))
        )
      ;; FIXME: remove above -----------------------------------.
      ))
  (if filename
      (if (file-readable-p filename)
	  (if (integerp line-number)
	      (if (> line-number 0)
		  (let ((line-count))
		    (if (setq line-count (realgud:file-line-count filename))
			(if (> line-count line-number)
			    (let* ((column-number
				    (realgud:file-column-from-string filename
								    line-number
								    source-text))
				   (source-buffer (find-file-noselect filename))
				   (source-mark))

			      ;; And you thought we'd never get around to
			      ;; doing something other than validation?
			      (with-current-buffer source-buffer
				(goto-char (point-min))
				;; FIXME also allow column number and byte offset
				(forward-line (1- line-number))
				(make-realgud-loc
				      :num           bp-num
				      :cmd-marker    cmd-marker
				      :filename      filename
				      :line-number   line-number
				      :column-number column-number
				      :source-text   source-text
				      :marker        (point-marker)
				      )
				))
			  ;; else
			  (format "File %s has only %d lines. (Line %d requested.)"
				  filename line-count line-number))
		      (format "Problem getting line count for file `%s'" filename)))
		(format "line number %s should be greater than 0" line-number))
	    (format "%s is not an integer" line-number))
	;; else
	(format "File named `%s' not readable" filename)))
  )

(provide-me "realgud-")
