(require 'load-relative)
(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")

(test-simple-start)

(note "dbgr-buffer-backtrace")

(assert-equal "abc" (dbgr-get-buffer-base-name "*abc*")
	      "remove buffer stars")

(assert-equal "abc" (dbgr-get-buffer-base-name "abc")
	      "no buffer stars")

(assert-equal "abc" (dbgr-get-buffer-base-name "*abc shell*")
	      "remove buffer stars and shell")

(end-tests)
