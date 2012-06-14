(require 'test-simple)
(load-file "../dbgr.el")

(test-simple-start)

(note "dbgr")

(note "dbgr-starts-with")
(assert-equal 1 (dbgr-string-starts-with "abcdef" "dbgr-"))
(assert-t (dbgr-string-starts-with "dbgr-foo" "dbgr-"))
(assert-equal -5 (dbgr-string-starts-with "dbgrfoo" "dbgr-"))

(note "dbgr-loaded-features")
(set (make-local-variable 'dbgr-features) (dbgr-loaded-features))
(dolist (feature '(dbgr-trepan dbgr-pydbgr
			       dbgr-core))
  (assert-t (not (not (member feature dbgr-features)))) )

(note "dbgr-unload-features")
(load-file "../dbgr.el")
(assert-nil (not (dbgr-loaded-features)))
(assert-nil (not (dbgr-unload-features)))
(assert-nil (dbgr-loaded-features))

(end-tests)
