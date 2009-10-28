(load-file "./behave.el")
(load-file "../dbgr-loc.el")
(load-file "../dbgr-file.el")

(behave-clear-contexts)

(lexical-let ((filename (symbol-file 'behave)))

  (context "dbgr-file-line-count: "
	   (tag file)
	   (specify "File not found"
		    (expect (dbgr-file-line-count "not-found-file") equal nil))
	   (specify "File found"
		    (expect (integerp (dbgr-file-line-count filename)) t))
	   )

  (context "dbgr-file-loc-from-line: "
	   (tag file)
	   (specify "File not found"
		    (expect (stringp (dbgr-file-loc-from-line "not-found-file" 5)) t))
	   (specify "invalid real line number"
		    (expect (stringp (dbgr-file-loc-from-line filename 5.5)) t))
	   (specify "negative number"
		    (expect (stringp (dbgr-file-loc-from-line filename -1)) t))
	   (specify "Line number too large for file"
		    (expect (stringp (dbgr-file-loc-from-line filename 10001)) t))
	   (specify "Line number too large for file"
		    (expect (dbgr-loc-p (dbgr-file-loc-from-line filename 30)) t))
  ))
(behave "file")

