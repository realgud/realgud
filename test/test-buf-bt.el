(require 'test-unit)

(load-file "../dbgr/common/buffer/backtrace.el")

(test-unit-clear-contexts)

(context "dbgr-buffer-backtrace"
	 (tag dbgr-buf-bt)
	 (specify "remove buffer stars"
		  (assert-equal "abc" 
				(dbgr-remove-surrounding-stars "*abc*")
				)
		  )
	 (specify "no buffer stars"
		  (assert-equal "abc" 
				(dbgr-remove-surrounding-stars "*abc*")
				)
		  )
	 )
(test-unit "dbgr-buf-bt")

