(require 'test-simple)
(load-file "../dbgr/common/helper.el")

(test-simple-start)

(defstruct dbgr-test-info name)
(dbgr-struct-field-setter "dbgr-test-info" "name")

(set (make-local-variable 'dbgr-test-info)
     (make-dbgr-test-info :name "foo"))

(note "setter macro works")
(assert-t (functionp 'dbgr-test-info-name=))
(assert-equal "foo" (dbgr-test-info-name= "foo"))

(end-tests)
