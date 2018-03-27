;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "./bt-helper.el")
(load-file "../realgud/debugger/trepan/init.el")

(declare-function setup-bt 'realgud-bt-helper)
(declare-function __FILE__ 'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar temp-bt)
)

(defun test-bt-trepan()
  (setq temp-bt
	(setup-bt "trepan"
		"--> #0 METHOD Object#gcd(a, b) in file /test/gcd.rb at line 4
    #1 TOP Object#<top /gcd.rb> in file /test/gcd.rb
	at line 19
"))

  (with-current-buffer temp-bt
    (switch-to-buffer temp-bt)
    (goto-char (point-min))
    (dolist (pair
	     '(
	       ("#" .     realgud-backtrace-number )
	       ("METHO" . font-lock-keyword-face )
	       ("Objec" . font-lock-constant-face )
	       ("#"     . font-lock-function-name-face )
	       ("("     . font-lock-variable-name-face )
	       ("/test" . realgud-file-name)
	       ("line " . realgud-line-number)
	       ("#"     . realgud-backtrace-number)
	       ("Objec" . font-lock-constant-face )
	       ("<top"  . font-lock-variable-name-face)
	       ("/test" . realgud-file-name)
	       ("line " . realgud-line-number)
	       ))
      (search-forward (car pair))
      (assert-equal (cdr pair)
		    (get-text-property (point) 'face))
      )
    ))

(test-bt-trepan)

(end-tests)
