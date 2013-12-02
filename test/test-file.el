(require 'test-simple)
(load-file "../realgud/common/loc.el")
(load-file "../realgud/common/file.el")

(declare-function realgud-file-loc-from-line   'realgud-file)
(declare-function realgud-file-line-count 'realgud-file)
(declare-function realgud-loc?(loc))

(test-simple-start)

(eval-when-compile
  (defvar realgud-file-remap)
  (defvar test-filename)
  (defvar remap-filename)
  (defvar old-compilation-find-file)
)

(clrhash realgud-file-remap)

(setq old-compilation-find-file (symbol-function 'compilation-find-file))

(setq test-filename (symbol-file 'test-simple))

(note "realgud-file-line-count")

(assert-nil
 (realgud-file-line-count "not-found-file")
 "File not found")

(assert-t (integerp (realgud-file-line-count test-filename))
	  "File found")


(note "realgud-file-loc-from-line")

(fset 'compilation-find-file (lambda(mark test-filename opt)
			       (get-buffer "*scratch*")))
(assert-t (equal (realgud-file-loc-from-line
		  "not-found-file" 5 (make-marker))
		 "File named `not-found-file' not readable"))

(assert-t (stringp (realgud-file-loc-from-line test-filename 5.5))
	  "invalid real line number")

(assert-t (stringp (realgud-file-loc-from-line test-filename -1))
	  "negative number")

(assert-t (stringp (realgud-file-loc-from-line test-filename 10001))
	  "Line number too large for file")

(assert-t (realgud-loc?
	   (realgud-file-loc-from-line test-filename 30))
	  "Ok loc creation - no cmd marker")

(assert-t (realgud-loc?
	   (realgud-file-loc-from-line test-filename 30 (make-marker)))
	  "Ok loc creation - cmd marker")

(note "realgud-file-loc-from-line remapping")

(setq remap-filename " bogus remap-filename.el")

(assert-t (equal (realgud-file-loc-from-line
		  remap-filename 5 (make-marker))
		 (format "File named `%s' not readable" remap-filename)))


(puthash remap-filename test-filename realgud-file-remap)

(assert-t (realgud-loc?
	   (realgud-file-loc-from-line remap-filename 30))
	  "Ok loc creation with remap - no cmd marker")

(fset 'compilation-find-file old-compilation-find-file)

(end-tests)
