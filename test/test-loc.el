(require 'test-simple)
(load-file "../dbgr/common/loc.el")

(test-simple-start)

(save-current-buffer

  ;; Below, we need to make sure current-buffer has an associated
  ;; file with it.
  (find-file (symbol-file 'test-simple))

  (note "location field extraction")
  (let* ((buff (current-buffer))
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
    
    (assert-equal 5 (dbgr-loc-line-number good-loc) "line-number extraction")
    
    (assert-equal source-marker (dbgr-loc-marker good-loc) 
		  "source code marker extraction")

    
    (assert-equal cmd-marker (dbgr-loc-cmd-marker good-loc)
		  "command process marker extraction")

    
    (dbgr-loc-marker= good-loc2 source-marker)
    (assert-equal source-marker (dbgr-loc-marker good-loc2)
		  "marker set")

    ))

(end-tests)

; TODO: add test for debug-loc-goto, e.g.
;(dbgr-loc-goto (dbgr-loc-new "/tmp/bashdb.diff" 8))
;(dbgr-loc-goto (dbgr-loc-new "/tmp/bashdb.diff" 8) 'other-window 1)


