(load-file "./behave.el")
(load-file "../shortkey.el")
(behave-clear-contexts)

(context "dbgr-shortkey"
	 (tag shortkey)
	 (specify "assert-raises"
		  (assert-raises error
		   		 (dbgr-shortkey-mode-setup 't))
		  )
)

(behave "shortkey")
