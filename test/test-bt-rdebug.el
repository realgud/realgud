(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")
(load-file "../dbgr/debugger/rdebug/init.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "./bt-helper.el")

(test-simple-start)

(note "fontify")
(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(setq temp-bt (generate-new-buffer "*bt-test*"))
(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (dbgr-cmdbuf-init temp-cmdbuf "rdebug" 
		    (gethash "rdebug" dbgr-pat-hash))
  
  (switch-to-buffer nil)
  )

(setup-bt 
 "--> #0 Object.gcd(a#Fixnum, b#Fixnum) 
       at line /test/gcd.rb:6
    #1 at line /test/gcd.rb:19
"
 temp-bt temp-cmdbuf)

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
  (switch-to-buffer nil)
  )

(end-tests)

