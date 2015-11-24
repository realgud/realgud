;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'load-relative)
(require 'test-simple)
(require 'font-lock)


(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/common/buffer/backtrace.el")

(declare-function realgud-get-buffer-base-name 'realgud-command)

(test-simple-start)

(note "realgud-buffer-backtrace")

(assert-equal "abc" (realgud-get-buffer-base-name "*abc*")
	      "remove buffer stars")

(assert-equal "abc" (realgud-get-buffer-base-name "abc")
	      "no buffer stars")

(assert-equal "abc" (realgud-get-buffer-base-name "*abc shell*")
	      "remove buffer stars and shell")

(end-tests)
