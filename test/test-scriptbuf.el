(load-file "./behave.el")
(load-file "../dbgr-scriptbuf.el")

(behave-clear-contexts)

(context "dbgr-scriptbuf"
	 (tag dbgr-scriptbuf)
	 (specify "dbgr-scriptbuf-command-string - uninit"
		  (assert-equal nil (dbgr-scriptbuf-command-string)))
	 (specify "dbgr-scriptbuf-init"
		  (dbgr-scriptbuf-init (current-buffer) (current-buffer)
				       "fake-debugger"
				       '("/bin/pdb" "--emacs" "fake.py" "1"))
		  (assert-equal "fake-debugger" 
				(dbgr-scriptbuf-info-name dbgr-scriptbuf-info)))
	 (specify "dbgr-scriptbuf-command-string"
		  (assert-equal "/bin/pdb --emacs fake.py 1"
				(dbgr-scriptbuf-command-string))
	 ))

(behave "dbgr-scriptbuf")

