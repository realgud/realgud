(load-file "./behave.el")
(load-file "../dbgr-load.el")

(defun setup()
  (setq loc-current (symbol-file 'dbgr-loc-current))
  (if loc-current 
      (progn 
	(fmakunbound 'dbgr-loc-current)
	(setq load-history '())
	(load-file "../dbgr-load.el")))
  (setq dbgr-loc-file (concat (dbgr-directory) "dbgr-loc.el")))

(behave-clear-contexts)

(context "rdbg-load"
	 (tag load)
	 (specify "Initialized history"
		  (setup)
		  (expect-nil (symbol-file 'dbgr-loc-current)))
	 (specify "Read el file when none exists"
		  (dbgr-require-relative "dbgr-loc.el" t)
		  (expect-equal dbgr-loc-file 
				(symbol-file 'dbgr-loc-current) equal))
	 (setup)
	 ;; Try byte compiling a file and then rereading
	 (setq dbgr-loc-file-compiled (concat dbgr-loc-file "c"))
	 (unless (file-readable-p dbgr-loc-file-compiled)
	   (byte-compile-file dbgr-loc-file))
	 (if (file-readable-p dbgr-loc-file-compiled)
	     (progn 
	       (dbgr-require-relative "dbgr-loc.elc" nil)
	       (specify "Read elc file when we already have read in an el file"
			(dbgr-require-relative (file-name-nondirectory
						 dbgr-loc-file-compiled))
			(expect-equal dbgr-loc-file-compiled
				      (symbol-file 'dbgr-loc-current)
				))
	       ;; (setup)
	       ;; (specify "Read el file by default when we already
	       ;; have read in an elc file"
	       ;; 		(dbgr-require-relative "rdbg-loc" t)
	       ;; 		(expect (symbol-file 'dbgr-loc-current) equal
	       ;; 			dbgr-loc-file))
	       )))

(behave "load")

