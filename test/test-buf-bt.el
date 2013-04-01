(require 'load-relative)
(require 'test-simple)
(require 'font-lock)

(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/common/buffer/backtrace.el")

(test-simple-start)

(note "realgud-buffer-backtrace")

(assert-equal "abc" (realgud-get-buffer-base-name "*abc*")
	      "remove buffer stars")

(assert-equal "abc" (realgud-get-buffer-base-name "abc")
	      "no buffer stars")

(assert-equal "abc" (realgud-get-buffer-base-name "*abc shell*")
	      "remove buffer stars and shell")

(end-tests)
