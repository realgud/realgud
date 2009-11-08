(load-file "./behave.el")
(load-file "../dbgr-scriptbuf.el")

(behave-clear-contexts)

(context "dbgr-scriptbuf"
	 (tag dbgr-scriptbuf)
	 (setq dbgr-scriptbuf-info nil)
	 (specify "dbgr-scriptbuf-command-string - uninit"
		  (assert-equal nil (dbgr-scriptbuf-command-string (current-buffer))))
	 (specify "dbgr-scriptbuf-init"
		  (dbgr-scriptbuf-init (current-buffer) (current-buffer)
				       "fake-debugger"
				       '("/bin/pdb" "--emacs" "fake.py" "1"))
		  (assert-equal "fake-debugger" 
				(dbgr-scriptbuf-info-name dbgr-scriptbuf-info)))
	 (specify "dbgr-scriptbuf-command-string"
		  (assert-equal "/bin/pdb --emacs fake.py 1"
				(dbgr-scriptbuf-command-string (current-buffer))))

	 (specify "dbgr-scriptbuf-init-or-update - update"
		  (dbgr-scriptbuf-init-or-update (current-buffer) (current-buffer))
		  (assert-equal (current-buffer)
				(dbgr-scriptbuf-info-cmdproc dbgr-scriptbuf-info)))
	 
	 (specify "dbgr-scriptbuf-init-or-update - init"
		  (setq dbgr-scriptbuf-info nil)
		  (dbgr-scriptbuf-init-or-update (current-buffer) (current-buffer))
		  (assert-equal (current-buffer)
				(dbgr-scriptbuf-info-cmdproc dbgr-scriptbuf-info)))
	 )

(behave "dbgr-scriptbuf")

