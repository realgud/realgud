(load-file "./behave.el")
(load-file "../dbgr-loc.el")
(load-file "../dbgr-lochist.el")

(behave-clear-contexts)

;;; (defun setup()
;;;      (lexical-let ((loc-hist (make-dbgr-loc-hist))
;;; 		   (filename (buffer-file-name (current-buffer)))
;;; 		   (loc (dbgr-loc-current)))
;;;        (dbgr-loc-hist-add loc-hist loc)))
;;;        ;; (message "aa ring-index %s" 
;;;        ;; 		(dbgr-loc-hist-index loc-hist))))

;;; (setup)


(lexical-let ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'behave))

  (context "location ring initialization and fields access: "
	   (tag lochist)
	   (lexical-let ((loc-hist (make-dbgr-loc-hist))
			 (filename (buffer-file-name (current-buffer)))
			 (loc (dbgr-loc-current)))
	     
	     (specify "get ring component for a new history ring"
		      (expect (ring-p (dbgr-loc-hist-ring loc-hist)) t))

	     (specify "ring position for an empty history ring is -1"
		      (expect (dbgr-loc-hist-position loc-hist)
			      equal -1))

	     (specify "get item for an empty history ring"
		      (expect (dbgr-loc-hist-item loc-hist) equal nil))
	     
	     (specify "add an item to an empty history ring"
		      (dbgr-loc-hist-add loc-hist loc)
		      (expect (dbgr-loc-hist-item loc-hist) equal loc))

	     (specify "One item in history ring"
		      (expect (ring-length 
			       (dbgr-loc-hist-ring loc-hist))
			      equal 1))

	     (specify "ring index in history ring is 1"
		      (expect (dbgr-loc-hist-index loc-hist) equal 1))

	     (specify "duplicate item added is ignored"
		      (dbgr-loc-hist-add loc-hist loc)
		      (expect (ring-length 
			       (dbgr-loc-hist-ring loc-hist)) 
			      equal 1))

	     (specify "ring index in history ring after dup ignore is still 1"
		      (expect (dbgr-loc-hist-index loc-hist) equal 1))


	     (specify "Set to newest position"
		      (expect (dbgr-loc-hist-newest loc-hist) equal -1))
	     
	     ))
  (behave "lochist")
  (switch-to-buffer saved-buffer))

