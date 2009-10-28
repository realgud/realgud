(eval-when-compile (require 'cl))

(defun dbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'dbgr-load)
		       (buffer-file-name (current-buffer))
		       )))
    (if file-name
        (expand-file-name (file-name-directory file-name))
      nil)))

(defun dbgr-require-relative (filename-list &optional source-first)
  "load FILENAME relative to `dbgr-directory' if FILENAME is
already loaded it is reloaded. FILENAME can also be a list of
strings in which case each element is loaded. Suffixes '.el' and
'.elc' are automatically appended to filenames. If optional
argument SOURCE-FILE is t, then we prefer the source source file
over a compiled name."
  (if (not (listp filename-list)) (setq filename-list (list filename-list)))
  (loop for filename in filename-list do 
	(lexical-let* ((ext (file-name-extension filename))
		       (file-name-short 
			(expand-file-name (concat (dbgr-directory) filename)))
		       (first-filename file-name-short)
		       (el  (concat file-name-short ".el"))
		       (elc (concat file-name-short ".elc"))
		       file-list ret-value)
	  (cond ((equal ext "el")
		 (setq first-filename file-name-short)
		 (setq el nil))
		((equal ext "elc")
		 (setq first-filename file-name-short)
		 (setq elc nil)))
	  
	  (if source-first 
	      (setq file-list (list first-filename el elc))
	    (setq file-list (list first-filename elc el)))
	  (loop for filename in file-list do
		(if (and filename (file-readable-p filename))
		    (progn 
		      (load filename)
		      (setq ret-value t)
		      (return))
		  ))
	  ret-value)
	))
  
(defun dbgr-load-all-files()
  (lexical-let ((file-list '("dbgr-regexp" "dbgr-procbuf-var" 
			     "dbgr-loc" "dbgr-lochist" "dbgr-file"
			     "dbgr-window" "dbgr-track" "dbgr-track-mode"))
		(filename)
		(full-filename))
    (setq load-path (cons (dbgr-directory) load-path))
    (loop for filename in file-list do
	  ;; We always want the source for now
	  (setq full-filename (format "%s%s.el" (dbgr-directory) filename))
	  (message "Rocky is loading %s" full-filename)
	  (load-file full-filename))
    (setq load-path (cdr load-path))
    ))
  
(provide 'dbgr-load)
