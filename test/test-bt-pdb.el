(require 'test-unit)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")
(load-file "../dbgr/common/init.el")
(load-file "../dbgr/debugger/pdb/init.el")

(test-unit-clear-contexts)

(defun setup-bt(string temp-bt temp-cmdbuf)
  (with-current-buffer temp-bt
    (dbgr-backtrace-mode temp-cmdbuf)
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (insert string)
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    ))


(context "dbgr-buffer-backtrace-pdb"
	 (tag dbgr-buf-bt-pdb)
	 (specify "fontify"
		  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
		  (with-current-buffer temp-cmdbuf
		    (dbgr-cmdbuf-init temp-cmdbuf "pdb" 
				      (gethash "pdb" dbgr-pat-hash))
		    
		    )
		  (setq temp-bt (generate-new-buffer "*bt-test*"))
		  (setup-bt 
"->0 gcd(a=3, b=5) called from file '/test/gcd.py' at line 28
##1 <module> execfile() file '/test/gcd.py' at line 41
"
			    temp-bt temp-cmdbuf)
		  (with-current-buffer temp-bt
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
		  )
	 )
(test-unit "dbgr-buf-bt-pdb")

