(require 'test-unit)
(load-file "../dbgr/common/shortkey.el")
(test-unit-clear-contexts)

(context "dbgr-shortkey"
	 (tag shortkey)
	 (specify "assert-raises"
		  (assert-raises error
		   		 (dbgr-shortkey-mode-setup 't))
		  )
)

(test-unit "shortkey")
