(require 'test-unit)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")

(test-unit-clear-contexts)

(context "dbgr-buffer-backtrace"
	 (tag dbgr-buf-bt)
	 (specify "remove buffer stars"
		  (assert-equal "abc" 
				(dbgr-get-buffer-base-name "*abc*")
				)
		  )
	 (specify "no buffer stars"
		  (assert-equal "abc" 
				(dbgr-get-buffer-base-name "abc")
				)
		  )
	 (specify "remove buffer stars and shell"
		  (assert-equal "abc" 
				(dbgr-get-buffer-base-name "*abc shell*")
				)
		  )
	 )
(test-unit "dbgr-buf-bt")

