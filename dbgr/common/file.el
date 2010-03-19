; Should dbgr-file-loc-from-line be here or elsewhere?
(require 'load-relative)
(require-relative-list '("helper" "loc") "dbgr-")

(fn-p-to-fn?-alias 'file-exists-p)
(declare-function file-exists?(file))

(defun dbgr-file-line-count(filename)
  "Return the number of lines in file FILENAME, or nil FILENAME can't be
found"
  (if (file-exists? filename)
      (let ((file-buffer (find-file-noselect filename)))
	(with-current-buffer-safe file-buffer
	  (line-number-at-pos (point-max))))
    nil))

(defun dbgr-file-loc-from-line(filename line-number &optional cmd-marker bp-num)
  "Return a dbgr-loc for FILENAME and LINE-NUMBER

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (file-exists? filename)
      (if (integerp line-number)
	  (if (> line-number 0)
	      (lexical-let ((line-count))
		(if (setq line-count (dbgr-file-line-count filename))
		    (if (> line-count line-number)
			; And you thought we'd never get around to
			; doing something other than validation? 
			(make-dbgr-loc 
			 :bp-num      bp-num
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
    (format "File named `%s' not found" filename)))

(provide-me "dbgr-")
