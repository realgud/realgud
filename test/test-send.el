;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)

(load-file "../realgud/common/send.el")
(load-file "../realgud/common/regexp.el")
(load-file "../realgud/debugger/trepan/init.el")

(declare-function realgud-srcbuf-init 'realgud-buffer-source)
(test-simple-start)

(eval-when-compile
  (defvar temp-cmdbuf nil)
  (defvar realgud-pat-hash)
  (defvar file-name)
)

(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (realgud-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" realgud-pat-hash))
  (realgud-srcbuf-init (current-buffer) temp-cmdbuf)
)

(defun tear-down()
  (kill-buffer temp-cmdbuf)
)

(dolist (str '("abc" "100%" "I feel %% today"))
  (assert-equal str (realgud-expand-format str "format no expand characters")))


(assert-equal "line 5" (realgud-expand-format "line %p" 5)
	      "format %l - with arg")
(assert-equal "line " (realgud-expand-format "line %p")
	      "format %l - without arg")

(assert-equal "hi, rocky!"
	      (realgud-expand-format "h%s!" "i, rocky")
	      "format %s")

(setup)
;; Current buffer is now set up as a source buffer
(setq file-name (buffer-file-name))
(note "File formatting")
(if (and file-name (realgud-get-srcbuf (current-buffer)))
    (dolist
	(pair
	 (list
	  (cons "%d" (file-name-directory file-name))
	  (cons "%x" file-name)
	  (cons "%X" (expand-file-name file-name))
	  (cons "%f" "test-send.el")
	  (cons "%F" "test-send")))
      (assert-equal (cdr pair) (realgud-expand-format (car pair)))))
(tear-down)


(assert-raises error (realgud-command "testing"))

(end-tests)
