(load-file "./behave.el")
(load-file "../dbgr-loc.el")

(behave-clear-contexts)

(save-current-buffer

  ;; Below, we need to make sure current-buffer has an associated
  ;; file with it.
  (find-file (symbol-file 'behave))

  (context "location field extraction"
	   (tag loc)
	   (lexical-let* ((filename (buffer-file-name (current-buffer)))
			  (marker (point-marker))
			  (good-loc (make-dbgr-loc 
				     :filename filename 
				     :line-number 5 
				     :marker marker))
			  (good-loc2 (make-dbgr-loc 
				      :filename filename 
				      :line-number 6))
			  (good-loc3 (dbgr-loc-current)))
	     
	     (specify "line-number extraction"
		      (expect-equal 5 (dbgr-loc-line-number good-loc)))
	     (specify "marker extraction"
		      (expect-equal marker (dbgr-loc-marker good-loc)))

	     (specify "marker set"
		      (dbgr-loc-marker= good-loc2 marker)
		      (expect-equal marker (dbgr-loc-marker good-loc2)))

	     )))

(behave "loc")

; TODO: add test for debug-loc-goto, e.g.
;(dbgr-loc-goto (dbgr-loc-new "/tmp/bashdb.diff" 8))
;(dbgr-loc-goto (dbgr-loc-new "/tmp/bashdb.diff" 8) 'other-window 1)


