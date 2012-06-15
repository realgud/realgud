(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")
(load-file "../dbgr/debugger/pdb/init.el")

(test-simple-start)

(defun setup-bt(string temp-bt temp-cmdbuf)
  (with-current-buffer temp-bt
    (dbgr-backtrace-mode temp-cmdbuf)
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (insert string)
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    ))


(note "fontify")
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
    (switch-to-buffer temp-bt)
    (search-forward (car pair))
    (assert-eql (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)

