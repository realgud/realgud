(load-file "./bt-helper.el")
(load-file "../realgud/debugger/rdebug/init.el")

(declare-function setup-bt 'realgud-bt-helper)

(test-simple-start)

(eval-when-compile
  (defvar temp-bt)
)

(setq temp-bt
      (setup-bt "rdebug"
		"--> #0 Object.gcd(a#Fixnum, b#Fixnum)
       at line /test/gcd.rb:6
    #1 at line /test/gcd.rb:19
"))

(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair
	   '(
	     ("#" .     realgud-backtrace-number )
	     ("Objec" . font-lock-constant-face )
	     ("gc"    . font-lock-function-name-face )
	     ("("     . font-lock-variable-name-face )
	     ("Fixnu" . font-lock-constant-face )
	     ("/test" . realgud-file-name)
	     (":"     . realgud-line-number)
	     ("#"     . realgud-backtrace-number)
	     ("/test" . realgud-file-name)
	     (":"     . realgud-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)
