;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/gdb/core.el")

(declare-function realgud:gdb-parse-cmd-args 'realgud-gdb-core)

(test-simple-start)

(note "invoke gdb without command line arguments")
(assert-equal '(("gdb") nil nil nil)
	      (realgud:gdb-parse-cmd-args
	       '("gdb")))

(note "invoke gdb with annotate command line parameter")
(assert-equal '(("gdb" "--annotate" "1") nil nil t)
	      (realgud:gdb-parse-cmd-args
	       '("gdb" "--annotate" "1")))

(note "invoke gdb with annotate command line parameter and file")
(assert-equal '(("gdb" "--annotate" "1") nil ("file.c") t)
	      (realgud:gdb-parse-cmd-args
	       '("gdb" "--annotate" "1" "file.c")))

(note "invoke gdb with annotate command line parameter and pid")
(assert-equal '(("gdb" "--annotate" "1" "-p") nil ("4812") t)
	      (realgud:gdb-parse-cmd-args
	       '("gdb" "--annotate" "1" "-p" "4812")))

(note "invoke gdb with pid")
(assert-equal '(("gdb" "-p") nil ("4511") nil)
	      (realgud:gdb-parse-cmd-args
	       '("gdb" "-p" "4511")))

(end-tests)
