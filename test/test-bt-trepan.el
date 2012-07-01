(load-file "./bt-helper.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-simple-start)

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
	     ("#" .     dbgr-backtrace-number )
	     ("METHO" . font-lock-keyword-face )
	     ("Objec" . font-lock-constant-face )
	     ("#"     . font-lock-function-name-face )
	     ("("     . font-lock-variable-name-face )
	     ("/test" . dbgr-file-name)
	     ("line " . dbgr-line-number)
	     ("#"     . dbgr-backtrace-number)
	     ("Objec" . font-lock-constant-face )
	     ("<top"  . font-lock-variable-name-face)
	     ("/test" . dbgr-file-name)
	     ("line " . dbgr-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)
