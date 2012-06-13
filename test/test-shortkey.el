(require 'test-simple)
(load-file "../dbgr/common/shortkey.el")
(test-simple-start)

(note "dbgr-shortkey")
(assert-raises error (dbgr-shortkey-mode-setup 't))

(end-tests)
