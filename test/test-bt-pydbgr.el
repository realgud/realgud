(load-file "./bt-helper.el")
(load-file "../dbgr/debugger/pydbgr/init.el")

(test-simple-start)

(setq temp-bt
      (setup-bt "pydbgr"
		"->0 gcd(a=3, b=5) called from file '/test/gcd.py' at line 28
##1 <module> execfile() file '/test/gcd.py' at line 41
"))

(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair 
	   '(
	     ("->" .    dbgr-backtrace-number )
	     ("gc"    . font-lock-function-name-face )
	     ("("     . font-lock-variable-name-face )
	     ("/test" . dbgr-file-name)
	     ("2"     . dbgr-line-number)
	     ("##"    . dbgr-backtrace-number)
	     ("/test" . dbgr-file-name)
	     ("4"     . dbgr-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )
(end-tests)

