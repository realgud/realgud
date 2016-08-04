(eval-when-compile (require 'cl-lib))
(require 'test-simple)
(load-file "../realgud/common/helper.el")

(declare-function realgud-struct-field-setter 'realgud-helper)
(declare-function realgud:debugger-name-transform 'realgud-helper)
(declare-function __FILE__                    'load-relative)


(test-simple-start)

(eval-when-compile
  (defvar realgud-test-info)
)

(cl-defstruct realgud-test-info name)
(realgud-struct-field-setter "realgud-test-info" "name")

(set (make-local-variable 'realgud-test-info)
     (make-realgud-test-info :name "foo"))

(note "setter macro works")
(assert-t (functionp 'realgud-test-info-name=))
(assert-equal "foo" (realgud-test-info-name= "foo"))

(note "realgud:debugger-name-transform")
(assert-equal "trepan" (realgud:debugger-name-transform "trepan"))
;; (assert-equal "realgud:gdb" (realgud:debugger-name-transform "gdb"))

(end-tests)
