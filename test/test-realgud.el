(require 'test-simple)
(load-file "../realgud.el")

(declare-function realgud-loaded-features    'realgud)
(declare-function realgud-unload-features    'realgud-regexp)
(declare-function realgud-string-starts-with 'realgud-regexp)

(test-simple-start)

(eval-when-compile
  (defvar realgud-features)
)

(note "realgud")

(note "realgud-starts-with")
(assert-equal 1 (realgud-string-starts-with "abcdef" "realgud-"))
(assert-t (realgud-string-starts-with "realgud-foo" "realgud-"))
(assert-equal -8 (realgud-string-starts-with "realgudfoo" "realgud-"))

(note "realgud-loaded-features")
(set (make-local-variable 'realgud-features) (realgud-loaded-features))
(dolist (feature '(realgud-trepan realgud-pydbgr
			       realgud-core))
  (assert-t (not (not (member feature realgud-features)))) )

(note "realgud-unload-features")
(load-file "../realgud.el")
(assert-nil (not (realgud-loaded-features)))
(assert-nil (not (realgud-unload-features)))
(assert-nil (realgud-loaded-features))

(end-tests)
