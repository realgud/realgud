(require 'test-simple)
(load-file "../realgud/common/helper.el")

(test-simple-start)

(defstruct realgud-test-info name)
(realgud-struct-field-setter "realgud-test-info" "name")

(set (make-local-variable 'realgud-test-info)
     (make-realgud-test-info :name "foo"))

(note "setter macro works")
(assert-t (functionp 'realgud-test-info-name=))
(assert-equal "foo" (realgud-test-info-name= "foo"))

(end-tests)
