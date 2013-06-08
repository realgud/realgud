(require 'test-simple)
(load-file "../realgud/common/loc.el")
(load-file "../realgud/common/file.el")

(test-simple-start)

(clrhash realgud-file-remap)

(setq old-compilation-find-file (symbol-function 'compilation-find-file))

(setq filename (symbol-file 'test-simple))

(note "realgud-file-line-count")

(assert-nil
 (realgud-file-line-count "not-found-file")
 "File not found")

(assert-t (integerp (realgud-file-line-count filename))
	  "File found")


(note "realgud-file-loc-from-line")

(fset 'compilation-find-file (lambda(mark filename opt)
			       (get-buffer "*scratch*")))
(assert-t (equal (realgud-file-loc-from-line
		  "not-found-file" 5 (make-marker))
		 "File named `not-found-file' not readable"))

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

(note "realgud-file-loc-from-line remapping")

(setq remap-filename " bogus remap-filename.el")

(assert-t (equal (realgud-file-loc-from-line
		  remap-filename 5 (make-marker))
		 (format "File named `%s' not readable" remap-filename)))


(puthash remap-filename filename realgud-file-remap)

(assert-t (realgud-loc-p
	   (realgud-file-loc-from-line remap-filename 30))
	  "Ok loc creation with remap - no cmd marker")

(fset 'compilation-find-file old-compilation-find-file)

(end-tests)
