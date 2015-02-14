(require 'test-simple)
(load-file "../realgud/common/utils.el")

(declare-function realgud:flatten            'realgud-utils)
(declare-function realgud:strip              'realgud-regexp)
(declare-function __FILE__                   'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar test-realgud:features)
)

(note "realgud:strip")
(assert-equal "abc" (realgud:strip "abc"))
(assert-equal "def" (realgud:strip "\n  def\t  "))

(note "realgud:flatten")
(assert-equal '(abc) (realgud:flatten '(abc)))
(assert-equal '(abc def h i j) (realgud:flatten '(abc (def (h) i) j)))

(end-tests)
