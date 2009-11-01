(load-file "./behave.el")
(load-file "../dbgr-loc.el")
(load-file "../dbgr-file.el")

(behave-clear-contexts)

(lexical-let ((filename (symbol-file 'behave)))

  (context "dbgr-file-line-count"
	   (tag file)
	   (specify "File not found"
		    (assert-nil
		     (dbgr-file-line-count "not-found-file")))
	   (specify "File found"
		    (assert-t (integerp (dbgr-file-line-count filename))))
	   )

  (context "dbgr-file-loc-from-line"
	   (tag file)
	   (specify "File not found"
		    (assert-t (stringp (dbgr-file-loc-from-line 
					"not-found-file" 5))))
	   (specify "invalid real line number"
		    (assert-t (stringp (dbgr-file-loc-from-line filename 5.5))))
	   (specify "negative number"
		    (assert-t (stringp (dbgr-file-loc-from-line filename -1))))
	   (specify "Line number too large for file"
		    (assert-t (stringp (dbgr-file-loc-from-line filename 10001))))
	   (specify "Line number too large for file"
		    (assert-t (dbgr-loc-p 
			       (dbgr-file-loc-from-line filename 30))))
  ))
(behave "file")

