(require 'test-simple)
(load-file "../realgud/common/shortkey.el")
(test-simple-start)

(note "realgud-shortkey")
(assert-raises error (realgud-shortkey-mode-setup 't))

(end-tests)
