(load-file "./bt-helper.el")
(load-file "../dbgr/debugger/rdebug/init.el")

(test-simple-start)

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
	     ("#" .     dbgr-backtrace-number )
	     ("Objec" . font-lock-constant-face )
	     ("gc"    . font-lock-function-name-face )
	     ("("     . font-lock-variable-name-face )
	     ("Fixnu" . font-lock-constant-face )
	     ("/test" . dbgr-file-name)
	     (":"     . dbgr-line-number)
	     ("#"     . dbgr-backtrace-number)
	     ("/test" . dbgr-file-name)
	     (":"     . dbgr-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)

