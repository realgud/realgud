(load-file "./behave.el")
(load-file "../dbgr-cmdbuf.el")

(behave-clear-contexts)


(context "dbgr-cmdbuf"
	 (tag dbgr-cmdbuf)
	 (specify "dbgr-cmdbuf? before init"
		  (assert-nil (dbgr-cmdbuf? (current-buffer))))

	 (specify "dbgr-cmdbuf-command-string - uninit"
	 	  (assert-equal nil (dbgr-cmdbuf-command-string (current-buffer))))
	 ;; (specify "dbgr-cmdbuf-init"
	 ;; 	  (dbgr-cmdbuf-init (current-buffer) (current-buffer)
	 ;; 			    "fake-debugger"
	 ;; 			    '("/bin/pdb" "--emacs" "fake.py" "1"))
	 ;; 	  (assert-equal "fake-debugger" 
	 ;; 			(dbgr-cmdbuf-info-debugger-name dbgr-cmdbuf-info)))

	 ;; (specify "dbgr-cmdbuf? after init"
	 ;; 	  (assert-t (dbgr-cmdbuf? (current-buffer))))

	 ;; (specify "dbgr-cmdbuf-command-string"
	 ;; 	  (assert-equal "/bin/pdb --emacs fake.py 1"
	 ;; 			(dbgr-cmdbuf-command-string (current-buffer))))

	 ;; (specify "dbgr-cmdbuf-init-or-update - update"
	 ;; 	  (dbgr-cmdbuf-init-or-update (current-buffer) (current-buffer))
	 ;; 	  (assert-equal (current-buffer)
	 ;; 			(dbgr-cmdbuf-info-cmdproc dbgr-cmdbuf-info)))
	 
	 ;; (specify "dbgr-cmdbuf-init-or-update - init"
	 ;; 	  (setq dbgr-cmdbuf-info nil)
	 ;; 	  (dbgr-cmdbuf-init-or-update (current-buffer) (current-buffer))
	 ;; 	  (assert-equal (current-buffer)
	 ;; 			(dbgr-cmdbuf-info-cmdproc dbgr-cmdbuf-info)))
	 )

(behave "dbgr-cmdbuf")

