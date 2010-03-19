(require 'test-unit)
(load-file "../dbgr/common/loc.el")
(load-file "../dbgr/common/lochist.el")

(test-unit-clear-contexts)

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
  (find-file (symbol-file 'test-unit))

  (context "location ring initialization and fields access"
	   (tag lochist)
	   (lexical-let* ((loc-hist (make-dbgr-loc-hist))
			  (source-buffer (current-buffer))
			  (cmd-marker (point-marker))
			  (filename (buffer-file-name (current-buffer)))
			  (loc (dbgr-loc-current source-buffer cmd-marker)))
	     
	     (specify "get ring component for a new history ring"
		      (assert-t (ring-p (dbgr-loc-hist-ring loc-hist))))

	     (specify "ring position for an empty history ring is -1"
		      (assert-equal -1 (dbgr-loc-hist-position loc-hist)))

	     (specify "get item for an empty history ring"
		      (assert-nil (dbgr-loc-hist-item loc-hist)))
	     
	     (specify "add an item to an empty history ring"
		      (dbgr-loc-hist-add loc-hist loc)
		      (assert-equal loc (dbgr-loc-hist-item loc-hist)))

	     (specify "One item in history ring"
		      (assert-equal 1 (ring-length 
			       (dbgr-loc-hist-ring loc-hist))))

	     (specify "ring index in history ring is 1"
		      (assert-equal 1 (dbgr-loc-hist-index loc-hist)))

	     ;; (specify "duplicate item added is ignored"
	     ;; 	      (dbgr-loc-hist-add loc-hist loc)
	     ;; 	      (assert-equal 1 (ring-length 
	     ;; 		       (dbgr-loc-hist-ring loc-hist))))

	     (specify "ring index in history ring after dup ignore is still 1"
		      (assert-equal 1 (dbgr-loc-hist-index loc-hist)))


	     (specify "Set to newest position"
		      (assert-equal -1 (dbgr-loc-hist-newest loc-hist)))
	     
	     ))
  (test-unit "lochist")
  (switch-to-buffer saved-buffer))

