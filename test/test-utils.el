(require 'test-simple)
(load-file "../realgud/common/utils.el")

(declare-function realgud:flatten            'realgud-utils)
(declare-function realgud:strip              'realgud-regexp)
(declare-function realgud:string-starts-with 'realgud-utils)
(declare-function __FILE__                   'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar test-realgud:features)
)

(note "realgud:starts-with")
(assert-equal 1 (realgud:string-starts-with "abcdef" "realgud-"))
(assert-t (realgud:string-starts-with "realgud-foo" "realgud-"))
(assert-equal -8 (realgud:string-starts-with "realgudfoo" "realgud-"))

(note "realgud:strip")
(assert-equal "abc" (realgud:strip "abc"))
(assert-equal "def" (realgud:strip "\n  def\t  "))

(note "realgud:flatten")
(assert-equal '(abc) (realgud:flatten '(abc)))
(assert-equal '(abc def h i j) (realgud:flatten '(abc (def (h) i) j)))

(end-tests)
