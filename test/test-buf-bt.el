(require 'test-unit)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/trepan/init.el")

(test-unit-clear-contexts)

(context "dbgr-buffer-backtrace"
	 (tag dbgr-buf-bt)
	 (specify "remove buffer stars"
		  (assert-equal "abc" 
				(dbgr-remove-surrounding-stars "*abc*")
				)
		  )
	 (specify "no buffer stars"
		  (assert-equal "abc" 
				(dbgr-remove-surrounding-stars "*abc*")
				)
		  )
	 (specify "fontify"
		  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
		  (with-current-buffer temp-cmdbuf
		    (dbgr-cmdbuf-init temp-cmdbuf "trepan" 
				      (gethash "trepan" dbgr-pat-hash))
		    
		    )
		  (setq temp-bt (generate-new-buffer "*bt-test*"))
		  (with-current-buffer temp-bt
		    (dbgr-backtrace-mode temp-cmdbuf)
		    (goto-char (point-min))
		    (setq buffer-read-only nil)
		    (insert "--> #0 at line /usr/bin/irb:10\n")
		    (font-lock-fontify-buffer)
		    (goto-char (point-min))
		    (search-forward "--> #")
		    (assert-equal font-lock-constant-face 
				  (get-text-property (point) 'face))
		    )
		  )
	 )
(test-unit "dbgr-buf-bt")

