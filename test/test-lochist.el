(require 'test-simple)
(load-file "../dbgr/common/loc.el")
(load-file "../dbgr/common/lochist.el")

(test-simple-clear)

;;; (defun setup()
;;;      (lexical-let ((loc-hist (make-dbgr-loc-hist))
;;; 		   (filename (buffer-file-name (current-buffer)))
;;; 		   (loc (dbgr-loc-current)))
;;;        (dbgr-loc-hist-add loc-hist loc)))
;;;        ;; (message "aa ring-index %s" 
;;;        ;; 		(dbgr-loc-hist-index loc-hist))))

;;; (setup)


(let ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'test-simple))

  (note "location ring initialization and fields access")
  (let* ((loc-hist (make-dbgr-loc-hist))
	 (source-buffer (current-buffer))
	 (cmd-marker (point-marker))
	 (filename (buffer-file-name (current-buffer)))
	 (loc (dbgr-loc-current source-buffer cmd-marker)))
    
    (assert-t (ring-p (dbgr-loc-hist-ring loc-hist))
	      "get ring component for a new history ring")

    
    (assert-equal -1 (dbgr-loc-hist-position loc-hist)
		  "ring position for an empty history ring is -1")

    
    (assert-nil (dbgr-loc-hist-item loc-hist)
		"get item for an empty history ring")
	     
    (dbgr-loc-hist-add loc-hist loc)
    (assert-equal loc (dbgr-loc-hist-item loc-hist) 
		  "add an item to an empty history ring")

    
    (assert-equal 1 (ring-length 
		     (dbgr-loc-hist-ring loc-hist)) 
		  "One item in history ring")

    (assert-equal 1 (dbgr-loc-hist-index loc-hist)
		  "ring index in history ring is 1")

    ;; (dbgr-loc-hist-add loc-hist loc)
    ;; (assert-equal 1 (ring-length 
    ;; 		     (dbgr-loc-hist-ring loc-hist) )
    ;; 		  "duplicate item added is ignored")
    
    
    (assert-equal 1 (dbgr-loc-hist-index loc-hist)
		  "ring index in history ring after dup ignore is still 1")

    (assert-equal -1 (dbgr-loc-hist-newest loc-hist) "Set to newest position")
	     
	     ))

(end-tests)

