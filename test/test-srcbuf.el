(load-file "./behave.el")
(load-file "../dbgr-srcbuf.el")
(load-file "../dbgr-cmdbuf.el")

(behave-clear-contexts)

;; FIXME: use a real process buffer
(defun dbgr-cmdbuf-info-name(var) "foo")
(setq dbgr-cmdbuf-info (make-dbgr-cmdbuf-info))

(context "dbgr-srcbuf"
	 (tag dbgr-srcbuf)
	 (setq dbgr-srcbuf-info nil)
	 (specify "dbgr-srcbuf? before init"
		  (assert-nil (dbgr-srcbuf? (current-buffer))))

	 (specify "dbgr-srcbuf-command-string - uninit"
		  (assert-equal nil (dbgr-srcbuf-command-string (current-buffer))))
	 (specify "dbgr-srcbuf-init"
		  (dbgr-srcbuf-init (current-buffer) (current-buffer)
				       "fake-debugger"
				       '("/bin/pdb" "--emacs" "fake.py" "1"))
		  (assert-equal "fake-debugger" 
				(dbgr-srcbuf-info-debugger-name dbgr-srcbuf-info)))

	 (specify "dbgr-srcbuf? after init"
		  (assert-t (dbgr-srcbuf? (current-buffer))))

	 (specify "dbgr-srcbuf-command-string"
		  (assert-equal "/bin/pdb --emacs fake.py 1"
				(dbgr-srcbuf-command-string (current-buffer))))

	 (specify "dbgr-srcbuf-init-or-update - update"
		  (dbgr-srcbuf-init-or-update (current-buffer) (current-buffer))
		  (assert-equal (current-buffer)
				(dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info)))
	 
	 (specify "dbgr-srcbuf-init-or-update - init"
		  (setq dbgr-srcbuf-info nil)
		  (dbgr-srcbuf-init-or-update (current-buffer) (current-buffer))
		  (assert-equal (current-buffer)
				(dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info)))
	 )

(behave "dbgr-srcbuf")

