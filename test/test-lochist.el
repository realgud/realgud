(require 'test-simple)
(load-file "../realgud/common/loc.el")
(load-file "../realgud/common/lochist.el")

(test-simple-start)

;;; (defun setup()
;;;      (lexical-let ((loc-hist (make-realgud-loc-hist))
;;; 		   (filename (buffer-file-name (current-buffer)))
;;; 		   (loc (realgud-loc-current)))
;;;        (realgud-loc-hist-add loc-hist loc)))
;;;        ;; (message "aa ring-index %s"
;;;        ;; 		(realgud-loc-hist-index loc-hist))))

;;; (setup)


(let ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'test-simple))

  (note "location ring initialization and fields access")
  (let* ((loc-hist (make-realgud-loc-hist))
	 (source-buffer (current-buffer))
	 (cmd-marker (point-marker))
	 (filename (buffer-file-name (current-buffer)))
	 (loc (realgud-loc-current source-buffer cmd-marker)))

    (assert-t (ring-p (realgud-loc-hist-ring loc-hist))
	      "get ring component for a new history ring")


    (assert-equal -1 (realgud-loc-hist-position loc-hist)
		  "ring position for an empty history ring is -1")


    (assert-nil (realgud-loc-hist-item loc-hist)
		"get item for an empty history ring")

    (realgud-loc-hist-add loc-hist loc)
    (assert-equal loc (realgud-loc-hist-item loc-hist)
		  "add an item to an empty history ring")


    (assert-equal 1 (ring-length
		     (realgud-loc-hist-ring loc-hist))
		  "One item in history ring")

    (assert-equal 1 (realgud-loc-hist-index loc-hist)
		  "ring index in history ring is 1")

    ;; (realgud-loc-hist-add loc-hist loc)
    ;; (assert-equal 1 (ring-length
    ;; 		     (realgud-loc-hist-ring loc-hist) )
    ;; 		  "duplicate item added is ignored")


    (assert-equal 1 (realgud-loc-hist-index loc-hist)
		  "ring index in history ring after dup ignore is still 1")

    (assert-equal -1 (realgud-loc-hist-newest loc-hist) "Set to newest position")

	     ))

(end-tests)
