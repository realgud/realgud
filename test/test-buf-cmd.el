(require 'test-simple)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepan/init.el")
(test-simple-start)

(assert-nil (dbgr-cmdbuf? (current-buffer))
	    "dbgr-cmdbuf? before init")

(assert-equal nil (dbgr-cmdbuf-command-string (current-buffer))
	      "dbgr-cmdbuf-command-string - uninit")
(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(assert-t (dbgr-cmdbuf-init temp-cmdbuf "trepan" 
			    (gethash "trepan" dbgr-pat-hash))
	  "dbgr-cmdbuf-init")

(with-current-buffer temp-cmdbuf
  (switch-to-buffer temp-cmdbuf)
  (dbgr-cmdbuf-info-cmd-args= '("command" "args"))
  (assert-equal "command args" 
		(dbgr-cmdbuf-command-string temp-cmdbuf))
  (assert-equal "trepan" 
		(dbgr-cmdbuf-debugger-name))
  (assert-equal nil 
		(dbgr-cmdbuf-info-srcbuf-list 
		 dbgr-cmdbuf-info)
		"srcbuf-list should start out nil")
  (dbgr-cmdbuf-add-srcbuf (current-buffer) temp-cmdbuf)
  (assert-equal (list (current-buffer))
		(dbgr-cmdbuf-info-srcbuf-list
		 dbgr-cmdbuf-info)
		"should have added one item to srcbuf-list")
  (dbgr-cmdbuf-add-srcbuf (current-buffer) temp-cmdbuf)
  (assert-equal (list (current-buffer))
		(dbgr-cmdbuf-info-srcbuf-list
		 dbgr-cmdbuf-info)
		"Second source buffer same as first; should have added still only one item.")
  (switch-to-buffer nil)
  )

(end-tests)
