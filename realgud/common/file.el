;; Copyright (C) 2010-2011, 2013-2014, 2016-2020 Free Software Foundation, Inc

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
(require 'seq) ;; for seq-find seq-filter
(require-relative-list '("helper" "loc") "realgud-")
(require-relative-list '("buffer/command") "realgud-buffer-")

(declare-function realgud:strip         'realgud)
(declare-function realgud-loc-goto      'realgud-loc)
(declare-function realgud-get-cmdbuf    'realgud-buffer-helper)
(declare-function buffer-killed?        'helper)
(declare-function compilation-find-file 'compile)
(declare-function realgud-cmdbuf-info-ignore-re-file-list  'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-source-path=         'realgud-buffer-command)
(declare-function realgud-cmdbuf-mutex                     'realgud-buffer-command)
(declare-function realgud-cmdbuf-filename-remap-alist      'realgud-buffer-command)
(declare-function realgud-cmdbuf-filename-remap-alist=     'realgud-buffer-command)
(declare-function realgud-cmdbuf-mutex                     'realgud-buffer-command)

(defcustom realgud-file-find-function 'compilation-find-file
;;(defcustom realgud-file-find-function 'compilation-find-file
  "Function to call when we can't easily find file"
  :type 'function
  :group 'realgud)

(defun realgud--file-matching-suffix(paths suffix)
  (seq-filter (lambda (x) (string-suffix-p suffix x)) paths))

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

(defun realgud:file-ignore(filename ignore-re-file-list)
  (seq-find '(lambda (file-re) (string-match file-re filename)) ignore-re-file-list))

;; FIXME: should allow column number to be passed in.
(defun realgud:file-loc-from-line(filename line-number
					   &optional cmd-marker source-text bp-num
					   find-file-fn directory)
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

  (let* ((cmdbuf (or (realgud-get-cmdbuf) cmd-marker))
	 (ignore-re-file-list (realgud-cmdbuf-ignore-re-file-list cmdbuf))
	 (filename-remap-alist (realgud-cmdbuf-filename-remap-alist cmdbuf))
	 (remapped-filename
	  (assoc filename filename-remap-alist))
	 (mutex (realgud-cmdbuf-mutex cmdbuf))
	 (matching-file-list)
	 (buffer-files))

    ;;(with-mutex
    ;; mutex
     (when remapped-filename
       (if (file-readable-p (cdr remapped-filename))
	   (setq filename (cdr remapped-filename))
	 ;; else remove from map since no find
	 (realgud-cmdbuf-filename-remap-alist=
	  (delq (assoc remapped-filename filename-remap-alist)
					  filename-remap-alist))))

     (unless (and filename (file-readable-p filename))

       (with-current-buffer cmdbuf
	 (cond
	  ;; Is file already listed for ignore?
	  ((realgud:file-ignore filename ignore-re-file-list)
	   (message "tracking ignored for %s" filename))

	  ;; If we can find the filename, e.g. "src/code.c" as a suffix of file in
	  ;; the list of buffers seen, use that
	  ((and
	    (setq buffer-files
		  (with-current-buffer (marker-buffer cmd-marker)
		    (mapcar (lambda (buf) (buffer-file-name buf))
			    (realgud-cmdbuf-info-srcbuf-list realgud-cmdbuf-info))))
	    (setq matching-file-list (realgud--file-matching-suffix buffer-files filename))
	    (car matching-file-list)))

	  ;; Do we want to blacklist this?
	  ((y-or-n-p (format "Unable to locate %s\nBlacklist it for location tracking?" filename))
	   ;; FIXME: there has to be a simpler way to set ignore-file-list
	   (progn
	     (push filename ignore-re-file-list)
	     (realgud-cmdbuf-info-ignore-re-file-list= ignore-re-file-list)
	     (setq filename nil)
	   ))

	  ;; Do we have a custom find-file function?
	  (find-file-fn
	   (setq filename (funcall find-file-fn cmd-marker filename directory)))

	  (t
	   (let ((found-file (funcall realgud-file-find-function (point-marker) filename directory)))
	     (if found-file
		 (progn
		   (setq remapped-filename (buffer-file-name found-file))
		   (when (and remapped-filename (file-exists-p remapped-filename))
		     (realgud-cmdbuf-filename-remap-alist=
		      (cons
		       (cons filename remapped-filename)
		       filename-remap-alist)
		      cmdbuf)
		     (setq filename remapped-filename)
		     ))
	       ;; else
	       (setq filename nil)
	       )))
	  ))))
  ;;)

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

			      ;; Set this filename as the last one seen in cmdbuf
			      (realgud-cmdbuf-info-source-path= filename)

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
	(if filename
	    (format "File named `%s' not readable" filename))))
  )

;; FIXME: should allow column number to be passed in.
(defun realgud:file-remove-ignore(path-to-stop-ignoring)
  "Remove `path-to-stop-ignoring' from the list of paths which
are ignored in debugger location tracking. You might do this if you accidentllay
added a a path for ignoring by mistake."
  (interactive
   (list (completing-read "File name to stop ignoring: "
		    (realgud-cmdbuf-ignore-re-file-list (current-buffer))
		    nil t)))
  (when (member path-to-stop-ignoring (realgud-cmdbuf-ignore-re-file-list (current-buffer)))
    (realgud-cmdbuf-info-ignore-re-file-list=
     (delete path-to-stop-ignoring (realgud-cmdbuf-ignore-re-file-list (current-buffer)))))
  )


(provide-me "realgud-")
