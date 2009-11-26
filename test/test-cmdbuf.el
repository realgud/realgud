(load-file "./behave.el")
(load-file "../cmdbuf.el")
(load-file "../init.el")

(behave-clear-contexts)

(context "dbgr-cmdbuf"
	 (tag dbgr-cmdbuf)
	 (specify "dbgr-cmdbuf? before init"
		  (assert-nil (dbgr-cmdbuf? (current-buffer))))

	 (specify "dbgr-cmdbuf-command-string - uninit"
	 	  (assert-equal nil (dbgr-cmdbuf-command-string (current-buffer))))
	 (specify "dbgr-cmdbuf-init"
		  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
		  (assert-t (dbgr-cmdbuf-init temp-cmdbuf "rbdbgr" (gethash "rbdbgr" dbgr-pat-hash)))
		  (with-current-buffer temp-cmdbuf
 		    (dbgr-cmdbuf-info-cmd-args= '("command" "args"))
		    (assert-equal "command args" 
				  (dbgr-cmdbuf-command-string temp-cmdbuf))
		    (assert-equal "rbdbgr" 
				  (dbgr-cmdbuf-debugger-name))
		   ))
	 )

(behave "dbgr-cmdbuf")

