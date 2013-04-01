(require 'test-simple)
(load-file "../realgud/common/loc.el")
(load-file "../realgud/common/file.el")

(test-simple-start)

(setq filename (symbol-file 'test-simple))

(note "realgud-file-line-count")

(assert-nil
 (realgud-file-line-count "not-found-file")
 "File not found")

(assert-t (integerp (realgud-file-line-count filename))
	  "File found")


(note "realgud-file-loc-from-line")

;; (assert-t (stringp (realgud-file-loc-from-line
;; 		    "not-found-file" 5 (make-marker)))
;; 	  "File not found")

(assert-t (stringp (realgud-file-loc-from-line filename 5.5))
	  "invalid real line number")

(assert-t (stringp (realgud-file-loc-from-line filename -1))
	  "negative number")

(assert-t (stringp (realgud-file-loc-from-line filename 10001))
	  "Line number too large for file")

(assert-t (realgud-loc-p
	   (realgud-file-loc-from-line filename 30))
	  "Ok loc creation - no cmd marker")

(assert-t (realgud-loc-p
	   (realgud-file-loc-from-line filename 30 (make-marker)))
	  "Ok loc creation - cmd marker")

(end-tests)
