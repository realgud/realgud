;;; Copyright (C) 2010-2011, 2013-2014 Rocky Bernstein <rocky@gnu.org>
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
      (if (file-exists-p filename)
	  (let ((file-buffer (find-file-noselect filename)))
	    (with-current-buffer-safe file-buffer
	      (save-excursion
		(goto-char (point-min))
		(forward-line (1- line-number))
		(unless no-strip-blanks
		  (setq source-text (realgud:strip source-text)))
		(if (search-forward source-text (point-at-eol))
		    (- (current-column)
		       (length source-text))))))
	;; else
	nil)
    (error nil))
)


;; FIXME: should allow column number to be passed in.
(defun realgud:file-loc-from-line(filename line-number
					   &optional cmd-marker source-text bp-num
					   ;; FIXME: remove ignore-file-re and cover with
					   ;; find-file-fn.
					   ignore-file-re find-file-fn)
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
	  (message "tracking ignored for psuedo-file %s" filename)
	;; else
	(let ((remapped-filename))
	  (if (gethash filename realgud-file-remap)
	      (progn
		(setq remapped-filename (gethash filename realgud-file-remap))
		(if (file-exists-p remapped-filename)
		    (setq filename remapped-filename)
		  (remhash filename realgud-file-remap)))
	    ;; else
	    (progn
	      (setq remapped-filename
		    (buffer-file-name
		     (compilation-find-file (point-marker) filename nil)))
	      (when (and remapped-filename (file-exists-p remapped-filename))
		(puthash filename remapped-filename realgud-file-remap)
		(setq filename remapped-filename)
		)
	      )))
	)
      ;; FIXME: remove above -----------------------------------.
      ))
  (if filename
      (if (file-readable-p filename)
	  (if (integerp line-number)
	      (if (> line-number 0)
		  (lexical-let ((line-count))
		    (if (setq line-count (realgud:file-line-count filename))
			(if (> line-count line-number)
			    (let* ((column-number
				    (realgud:file-column-from-string filename
								    line-number
								    source-text))
				   ;; And you thought we'd never get around to
				   ;; doing something other than validation?
				   (loc (make-realgud-loc
					 :num           bp-num
					 :cmd-marker    cmd-marker
					 :filename      filename
					 :line-number   line-number
					 :column-number column-number
					 :source-text   source-text
					 :marker        (make-marker)
					 )))
			      loc)
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
