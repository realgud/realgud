;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/trepan3k/trepan3k.el")
(declare-function trepan3k-parse-cmd-args 'realgud:trepan3k)
(declare-function __FILE__                'require-relative)

(test-simple-start)

(note "trepan3k-parse-cmd-args")

(assert-equal '(nil ("trepan3k") ("foo") nil)
	      (trepan3k-parse-cmd-args '("trepan3k" "foo")))
(assert-equal '(nil ("trepan3k" "-n") ("foo") nil)
	      (trepan3k-parse-cmd-args '("trepan3k" "-n" "foo")))
(assert-equal '(nil ("trepan3k" "--annotate=1") ("foo") t)
	      (trepan3k-parse-cmd-args
	       '("trepan3k" "--annotate=1" "foo")))
(assert-equal '(nil ("mytrepan3k" "--annotate=1") ("foo") t)
	      (trepan3k-parse-cmd-args
	       '("mytrepan3k" "--annotate=1" "foo")))
(assert-equal '(("python") ("trepan3k" "--annotate") ("1" "foo") t)
	      (trepan3k-parse-cmd-args
	       '("python" "trepan3k" "--annotate" "1" "foo")))
(assert-equal '(("/usr/bin/python") ("trepan3k" "--different")
		("foo") nil)
	      (trepan3k-parse-cmd-args
	       '("/usr/bin/python" "trepan3k"
		 "--different" "foo")))
(assert-equal '(nil ("program.py") ("foo") nil)
	      (trepan3k-parse-cmd-args '("program.py" "foo")))
(assert-equal '(nil ("trepan3k") ("program.py" "foo") nil)
	      (trepan3k-parse-cmd-args
	       '("trepan3k" "program.py" "foo")))

(end-tests)
