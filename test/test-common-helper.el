(require 'test-unit)
(load-file "../dbgr/common/helper.el")

(test-unit-clear-contexts)

(defstruct dbgr-test-info name)
(dbgr-struct-field-setter "dbgr-test-info" "name")

(context "common-helper"
	 (tag common-helper)
	 
	 (set (make-local-variable 'dbgr-test-info)
	      (make-dbgr-test-info :name "foo"))
	 
	 (specify "setter macro works"
		  (assert-t (functionp 'dbgr-test-info-name=))
		  (assert-equal "foo" (dbgr-test-info-name= "foo")
				)
		  )
	 )

(test-unit "common-helper")
