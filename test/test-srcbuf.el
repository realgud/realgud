(require 'test-unit)
(load-file "../dbgr/common/buffer/source.el")
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-unit-clear-contexts)

(defvar temp-cmdbuf nil)
(defun tear-down()
  (kill-buffer temp-cmdbuf)
  (kill-buffer temp-srcbuf)
)

(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (dbgr-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" dbgr-pat-hash))
  (setq temp-srcbuf (find-file-noselect "./gcd.rb"))
)

(context "dbgr-srcbuf"
	 (tag dbgr-srcbuf)
	 (specify "dbgr-srcbuf? before init"
		  (assert-nil (dbgr-srcbuf? (current-buffer))))

	 (setq dbgr-srcbuf-info nil)
	 (specify "dbgr-srcbuf? before init - but nil"
		  (assert-nil (dbgr-srcbuf? (current-buffer))))

	 (specify "dbgr-srcbuf-command-string - uninit"
		  (assert-equal nil (dbgr-srcbuf-command-string (current-buffer))))

	 (specify "dbgr-srcbuf-init"
		  (setup)
	 	  (dbgr-srcbuf-init temp-srcbuf temp-cmdbuf
	 			       "trepan"
	 			       '("/bin/trepan" "--emacs" "gcd.rb" "1"))
	 	  (assert-equal "trepan" 
	 	  		(with-current-buffer temp-srcbuf
	 	  		  (dbgr-srcbuf-info-debugger-name 
	 	  		   dbgr-srcbuf-info)))

	 	  (assert-t (dbgr-srcbuf? temp-srcbuf)
			    "dbgr-srcbuf? after init")

		  (assert-equal "/bin/trepan --emacs gcd.rb 1"
		  		(dbgr-srcbuf-command-string 
		  		 temp-srcbuf)
		  		 "dbgr-srcbuf-command-string")

	 	  (assert-equal temp-cmdbuf
	 	  		(with-current-buffer temp-srcbuf
	 	  		  (dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info)))

	 	  (dbgr-srcbuf-init-or-update temp-srcbuf temp-cmdbuf)
	 	  (assert-equal temp-cmdbuf
	 	  		(with-current-buffer temp-srcbuf
	 	  		  (dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info))
		  		"dbgr-srcbuf-init-or-update - update")
	 
	 	  (kill-buffer temp-srcbuf)
	 	  (setq temp-srcbuf (find-file-noselect "./gcd.rb"))
	 	  (dbgr-srcbuf-init-or-update temp-srcbuf temp-cmdbuf)
	 	  (assert-equal temp-cmdbuf
	 	  		(with-current-buffer temp-srcbuf
	 	  		  (dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info))
		  		"dbgr-srcbuf-init-or-update - init")
		  (tear-down)
		  )
	 )

(test-unit "dbgr-srcbuf")

