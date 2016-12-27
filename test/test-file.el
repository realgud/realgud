;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/loc.el")
(load-file "../realgud/common/file.el")

;; Note the below line number is tested so it must match what's listed
;; below.
(declare-function __FILE__                     'load-relative)

(declare-function realgud:file-loc-from-line   'realgud-file)
(declare-function realgud:file-line-count      'realgud-file)
(declare-function realgud:file-column-from-string 'realgud-file)
(declare-function realgud-loc?                 'realgud-loc)

(declare-function realgud-loc-line-number      'realgud-loc)
(declare-function realgud-loc-column-number    'realgud-loc)
(declare-function realgud-loc-filename         'realgud-loc)

(test-simple-start)

(eval-when-compile
  (defvar realgud-file-remap)
  (defvar test-filename)
  (defvar test-file-loc)
  (defvar remap-filename)
  (defvar old-compilation-find-file)
)

(clrhash realgud-file-remap)

(setq old-compilation-find-file (symbol-function 'compilation-find-file))

(setq test-filename (symbol-file 'test-simple))

(note "realgud:file-line-count")

(assert-nil
 (realgud:file-line-count "not-found-file")
 "File not found")

(assert-t (integerp (realgud:file-line-count test-filename))
	  "File found")


(note "realgud:file-loc-from-line")

(fset 'compilation-find-file (lambda(mark test-filename opt)
			       (get-buffer "*scratch*")))

(save-excursion
  ;; NOTE: this calls compilation-find-file which prompts for a file
  ;; (assert-equal
  ;;  "File named `not-found-file' not readable"
  ;;  (realgud:file-loc-from-line
  ;;   "not-found-file" 5 (make-marker))
  ;;  )

  (assert-t (stringp (realgud:file-loc-from-line test-filename 5.5))
	    "invalid real line number")

  (assert-t (stringp (realgud:file-loc-from-line test-filename -1))
	    "negative number")

  (note "realgud:file-loc-from-line information")

  (assert-t (stringp (realgud:file-loc-from-line test-filename 10001))
	    "Line number too large for file")

  (setq test-file-loc (realgud:file-loc-from-line (__FILE__) 5 nil ";; Note"))
  (assert-t (realgud-loc? test-file-loc)
	    "Ok loc creation - no cmd marker")

  (assert-t (realgud-loc?
	     (realgud:file-loc-from-line test-filename 30 (make-marker)))
	    "Ok loc creation - cmd marker")

  (assert-equal 5 (realgud-loc-line-number test-file-loc))

  ;; FIXME: don't know why this fails in batch
  ;; (assert-equal 0 (realgud-loc-column-number test-file-loc))

  (assert-equal (__FILE__) (realgud-loc-filename test-file-loc))

  (note "realgud:file-loc-from-line remapping")

  (setq remap-filename " bogus remap-filename.el")

  ;; (assert-equal
  ;;  (format "File named `%s' not readable" remap-filename)
  ;;  (realgud:file-loc-from-line
  ;;   remap-filename 5 (make-marker))
  ;;  )
  )


(puthash remap-filename test-filename realgud-file-remap)

(assert-t (realgud-loc?
	   (realgud:file-loc-from-line remap-filename 30))
	  "Ok loc creation with remap - no cmd marker")

;; FIXME: don't know why this fails in batch
;; (assert-equal
;;  18
;;  (realgud:file-column-from-string (__FILE__) 7 "__FILE__")
;;  "Should find string in file/line and get column"
;;  )

(assert-nil
 (realgud:file-column-from-string (__FILE__) 5 "__FILE__")
 "Should not find string at this line in located file"
 )

(assert-nil
 (realgud:file-column-from-string (concat (__FILE__) "FOO") 7 "__FILE__")
 "Should not find file"
 )

(assert-nil
 (realgud:file-column-from-string (__FILE__) 10000 "__FILE__")
 "Should not find line in file at all"
 )

(fset 'compilation-find-file old-compilation-find-file)

(end-tests)
