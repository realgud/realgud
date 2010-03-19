(require 'test-unit)
(load-file "../dbgr/common/loc.el")
(load-file "../dbgr/common/file.el")

(test-unit-clear-contexts)

(lexical-let ((filename (symbol-file 'test-unit)))
  
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
					"not-found-file" 5 (make-marker)))))
	   (specify "invalid real line number"
		    (assert-t (stringp (dbgr-file-loc-from-line filename 5.5))))
	   (specify "negative number"
		    (assert-t (stringp (dbgr-file-loc-from-line filename -1))))
	   (specify "Line number too large for file"
		    (assert-t (stringp (dbgr-file-loc-from-line filename 10001))))
	   (specify "Ok loc creation - no cmd marker"
		    (assert-t (dbgr-loc-p 
			       (dbgr-file-loc-from-line filename 30))))
	   (specify "Ok loc creation - cmd marker"
		    (assert-t (dbgr-loc-p 
			       (dbgr-file-loc-from-line filename 30 (make-marker)))))
  ))
(test-unit "file")
