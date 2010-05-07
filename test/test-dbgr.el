(require 'test-unit)
(load-file "../dbgr.el")

(test-unit-clear-contexts)

(context "dbgr"
	 (tag dbgr)
	 (specify "dbgr-starts-with"
		  (assert-equal 1 (dbgr-string-starts-with "abcdef" "dbgr-"))
		  (assert-t (dbgr-string-starts-with "dbgr-foo" "dbgr-"))
		  (assert-equal -5 (dbgr-string-starts-with "dbgrfoo" "dbgr-"))
		  )
	 (specify "dbgr-loaded-features"
		  (setq dbgr-features (dbgr-loaded-features))
		  (dolist (feature '(dbgr-rbdbgr dbgr-pydbgr dbgr-init-bashdb
						 dbgr-core))
		    (assert-t (not (not (member feature dbgr-features))))
		    )
		  )
	 (specify "dbgr-unload-features"
		  (load-file "../dbgr.el")
		  (assert-nil (not (dbgr-loaded-features)))
		  (assert-nil (not (dbgr-unload-features)))
		  (assert-nil (dbgr-loaded-features))
		  )
	 )

(test-unit "dbgr")
