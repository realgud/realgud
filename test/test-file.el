(require 'test-simple)
(load-file "../dbgr/common/loc.el")
(load-file "../dbgr/common/file.el")

(test-simple-start)

(setq filename (symbol-file 'test-simple))
  
(note "dbgr-file-line-count")

(assert-nil
 (dbgr-file-line-count "not-found-file")
 "File not found")

(assert-t (integerp (dbgr-file-line-count filename))
	  "File found")


(note "dbgr-file-loc-from-line")

;; (assert-t (stringp (dbgr-file-loc-from-line 
;; 		    "not-found-file" 5 (make-marker)))
;; 	  "File not found")

(assert-t (stringp (dbgr-file-loc-from-line filename 5.5))
	  "invalid real line number")

(assert-t (stringp (dbgr-file-loc-from-line filename -1))
	  "negative number")

(assert-t (stringp (dbgr-file-loc-from-line filename 10001))
	  "Line number too large for file")

(assert-t (dbgr-loc-p 
	   (dbgr-file-loc-from-line filename 30))
	  "Ok loc creation - no cmd marker")

(assert-t (dbgr-loc-p 
	   (dbgr-file-loc-from-line filename 30 (make-marker)))
	  "Ok loc creation - cmd marker")

(end-tests)
