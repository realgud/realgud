(require 'test-simple)
(load-file "../realgud/common/helper.el")

(declare-function realgud-struct-field-setter 'realgud-helper)
(declare-function realgud-test-info-name= (__FILE__))

(test-simple-start)

(eval-when-compile
  (defvar realgud-test-info)
)

(defstruct realgud-test-info name)
(realgud-struct-field-setter "realgud-test-info" "name")

(set (make-local-variable 'realgud-test-info)
     (make-realgud-test-info :name "foo"))

(note "setter macro works")
(assert-t (functionp 'realgud-test-info-name=))
(assert-equal "foo" (realgud-test-info-name= "foo"))

(note "realgud:debugger-name-transform")
(assert-equal "trepan" (realgud:debugger-name-transform "trepan"))
(assert-equal "realgud:gdb" (realgud:debugger-name-transform "gdb"))

(end-tests)
