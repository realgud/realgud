(require 'test-unit)
(load-file "../dbgr/common/loc.el")

(test-unit-clear-contexts)

(save-current-buffer

  ;; Below, we need to make sure current-buffer has an associated
  ;; file with it.
  (find-file (symbol-file 'test-unit))

  (context "location field extraction"
	   (tag loc)
	   (lexical-let* ((buff (current-buffer))
			  (filename (buffer-file-name buff))
			  (source-marker (point-marker))
			  (cmd-marker (point-marker))
			  (good-loc (make-dbgr-loc 
				     :filename filename 
				     :line-number 5 
				     :marker source-marker
				     :cmd-marker cmd-marker
				     ))
			  (good-loc2 (make-dbgr-loc 
				      :filename filename 
				      :line-number 6
				     :marker source-marker
				     :cmd-marker cmd-marker
				     ))
			  (good-loc3 (dbgr-loc-current buff cmd-marker)))
	     
	     (specify "line-number extraction"
		      (assert-equal 5 (dbgr-loc-line-number good-loc)))
	     (specify "source code marker extraction"
		      (assert-equal source-marker (dbgr-loc-marker good-loc)))

	     (specify "command process marker extraction"
		      (assert-equal cmd-marker (dbgr-loc-cmd-marker good-loc)))

	     (specify "marker set"
		      (dbgr-loc-marker= good-loc2 source-marker)
		      (assert-equal source-marker (dbgr-loc-marker good-loc2)))

	     )))

(test-unit "loc")

; TODO: add test for debug-loc-goto, e.g.
;(dbgr-loc-goto (dbgr-loc-new "/tmp/bashdb.diff" 8))
;(dbgr-loc-goto (dbgr-loc-new "/tmp/bashdb.diff" 8) 'other-window 1)


