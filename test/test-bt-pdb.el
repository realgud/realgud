(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")
(load-file "../dbgr/debugger/pdb/init.el")
(load-file "./bt-helper.el")

(test-simple-start)

(note "fontify")
(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(setq temp-bt (generate-new-buffer "*bt-test*"))
(with-current-buffer temp-bt
  (switch-to-buffer temp-cmdbuf)
  (dbgr-cmdbuf-init temp-cmdbuf "pdb" 
		    (gethash "pdb" dbgr-pat-hash))
  (switch-to-buffer nil)
  )

(setup-bt 
 "->0 gcd(a=3, b=5) called from file '/test/gcd.py' at line 28
##1 <module> execfile() file '/test/gcd.py' at line 41
"
 temp-bt temp-cmdbuf)

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
    (assert-eql (cdr pair)
		  (get-text-property (point) 'face))
    )
  (switch-to-buffer nil)
  )

(end-tests)

