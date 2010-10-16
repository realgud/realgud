;; Programming language specific stuff.
(defun dbgr-lang-mode? (filename lang-str)
  "Return true if FILENAME is a buffer we are visiting a buffer
that is in LANG-STR mode. The test is just that the major mode
starts LANG-STR."
  (let ((buffer (and filename (find-buffer-visiting filename)))
	(match-pos))
    (if buffer 
	(progn 
	  (save-current-buffer
	    (set-buffer buffer)
	    (setq match-pos 
		  (string-match (format "^%s-" lang-str)
				(format "%s" major-mode))))
	  (and match-pos (= 0 match-pos)))
      nil)))

(defun dbgr-suggest-file-from-buffer (lang-str)
    "Suggest the first in the buffer list for which test-func is
    't. Typically this is used. To search for a buffer in one of
    the programming modes like Ruby or Python."
    (let ((file) 
	  (buff)
	  (not-found 't)
	  (buff-list (buffer-list)))
      (while (and not-found (setq buff (car-safe buff-list)))
	(setq buff-list (cdr buff-list))
	(setq file (buffer-file-name buff))
	(if (dbgr-lang-mode? file lang-str)
	    (setq not-found nil)
	  ))
      (if not-found nil file)
      )
)


(defun dbgr-suggest-ruby-file ()
 "Suggest a Ruby file to debug. First priority is given to the
current buffer. If the major mode is Ruby-mode, then we are
done. If the major mode is not Ruby, we'll use priority 2 and we
keep going.  Then we will try files in the default-directory. Of
those that we are visiting we will see if the major mode is Ruby,
the first one we find we will return.  Failing this, we see if the
file is executable and has a .rb suffix. These have priority 8.
Failing that, we'll go for just having a .rb suffix. These have
priority 7. And other executable files have priority 6.  Within a
given priority, we use the first one we find."
    (let* ((file)
	   (file-list (directory-files default-directory))
	   (priority 2)
	   (is-not-directory)
	   (result (buffer-file-name)))
      (if (not (dbgr-lang-mode? result "ruby"))
	  (progn 
	    (while (and (setq file (car-safe file-list)) (< priority 8))
	      (setq file-list (cdr file-list))
	      (if (dbgr-lang-mode? file "ruby")
		  (progn 
		    (setq result file)
		    (setq priority 
			  (if (file-executable-p file)
			      (setq priority 8)
			    (setq priority 7)))))
	      ;; The file isn't in a Ruby-mode buffer,
	      ;; Check for an executable file with a .rb extension.
	      (if (and file (file-executable-p file)
		       (setq is-not-directory (not (file-directory-p file))))
		  (if (and (string-match "\.rb$" file))
		      (if (< priority 6)
			  (progn
			    (setq result file)
			    (setq priority 6))))
		(if (and is-not-directory (< priority 5))
		    ;; Found some sort of executable file.
		    (progn
		      (setq result file)
		      (setq priority 5)))))
	    (if (and (< priority 6) 
		     (setq file (dbgr-suggest-file-from-buffer "ruby")))
		(setq result file))
	    ))
      result)
    )

;;; FIXME: DRY below with above.

(defun dbgr-suggest-python-file ()
    "Suggest a Python file to debug. First priority is given to the
current buffer. If the major mode is Python-mode, then we are
done. If the major mode is not Python, we'll use priority 2 and we
keep going.  Then we will try files in the default-directory. Of
those that we are visiting we will see if the major mode is Python,
the first one we find we will return.  Failing this, we see if the
file is executable and has a .py suffix. These have priority 8.
Failing that, we'll go for just having a .py suffix. These have
priority 7. And other executable files have priority 6.  Within a
given priority, we use the first one we find."
    (let* ((file)
	   (file-list (directory-files default-directory))
	   (priority 2)
	   (is-not-directory)
	   (result (buffer-file-name))
	   (buff-list (buffer-list))
	   (buff)
	   )
      (if (not (dbgr-lang-mode? result "python"))
	  (progn 
	    (while (and (setq file (car-safe file-list)) (< priority 8))
	      (setq file-list (cdr file-list))
	      (if (dbgr-lang-mode? file "python")
		  (progn 
		    (setq result file)
		    (setq priority 
			  (if (file-executable-p file)
			      (setq priority 8)
			    (setq priority 7)))))
	      ;; The file isn't in a Python-mode buffer,
	      ;; Check for an executable file with a .py extension.
	      (if (and file (file-executable-p file)
		       (setq is-not-directory (not (file-directory-p file))))
		  (if (and (string-match "\.py$" file))
		      (if (< priority 6)
			  (progn
			    (setq result file)
			    (setq priority 6))))
		(if (and is-not-directory (< priority 5))
		    ;; Found some sort of executable file.
		    (progn
		      (setq result file)
		      (setq priority 5)))))
	    (if (and (< priority 6) 
		     (setq file (dbgr-suggest-file-from-buffer "python")))
		(setq result file))
	    ))
      result)
    )



(provide-me "dbgr-")
