(load-file "./bt-helper.el")
(load-file "../realgud/debugger/pdb/init.el")

(declare-function setup-bt 'realgud-bt-helper)

(test-simple-start)

(eval-when-compile
  (defvar temp-bt)
)


(setq temp-bt
      (setup-bt "pdb"
		"->0 gcd(a=3, b=5) called from file '/test/gcd.py' at line 28
##1 <module> execfile() file '/test/gcd.py' at line 41
"))

(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair
	   '(
	     ("->" .    realgud-backtrace-number )
	     ("gc"    . font-lock-function-name-face )
	     ("("     . font-lock-variable-name-face )
	     ("/test" . realgud-file-name)
	     ("2"     . realgud-line-number)
	     ("##"    . realgud-backtrace-number)
	     ("/test" . realgud-file-name)
	     ("4"     . realgud-line-number)
	     ))
    (search-forward (car pair))
    (assert-eql (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)
