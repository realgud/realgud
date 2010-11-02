(require 'test-unit)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/init.el")
(load-file "../dbgr/zshdb/init.el")

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


(context "dbgr-buffer-backtrace-zshdb"
	 (tag dbgr-buf-bt-zshdb)
	 (specify "fontify"
		  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
		  (with-current-buffer temp-cmdbuf
		    (dbgr-cmdbuf-init temp-cmdbuf "zshdb" 
				      (gethash "zshdb" dbgr-pat-hash))
		    
		    )
		  (setq temp-bt (generate-new-buffer "*bt-test*"))
		  (setup-bt 
"->0 in file `/test/autogen.sh' at line 2
##1 /test/autogen.sh called from file `/usr/local/bin/zshdb' at line 121
"
			    temp-bt temp-cmdbuf)
		  (with-current-buffer temp-bt
		    (goto-char (point-min))
		    (dolist (pair 
			     '(
			       ("->" .    dbgr-backtrace-number )
			       ("/test" . dbgr-file-name)
			       ("line " . dbgr-line-number)
			       ))
		      (search-forward (car pair))
		      (assert-equal (cdr pair)
				    (get-text-property (point) 'face))
		      )
		    )
		  )
	 )
(test-unit "dbgr-buf-bt-zshdb")

