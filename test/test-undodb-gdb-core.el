;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/undodb-gdb/core.el")

(declare-function realgud:undodb-gdb-parse-cmd-args 'realgud-undodb-gdb-core)

(test-simple-start)

(note "invoke undodb-gdb without command line arguments")
(assert-equal '(("undodb-gdb") nil nil nil)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb")))

(note "invoke undodb-gdb with annotate command line parameter")
(assert-equal '(("undodb-gdb" "--annotate" "1") nil nil t)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb" "--annotate" "1")))

(note "invoke undodb-gdb with annotate command line parameter and file")
(assert-equal '(("undodb-gdb" "--annotate" "1") nil ("file.c") t)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb" "--annotate" "1" "file.c")))

(note "invoke undodb-gdb with annotate command line parameter and pid")
(assert-equal '(("undodb-gdb" "--annotate" "1" "-p") nil ("4812") t)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb" "--annotate" "1" "-p" "4812")))

(note "invoke undodb-gdb with pid")
(assert-equal '(("undodb-gdb" "-p") nil ("4511") nil)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb" "-p" "4511")))

(eval-when-compile
  (defvar test:warn-save)
  (defvar last-mess)
)

(setq test:warn-save (symbol-function 'warn))

(note "Stripping --interpreter=mi option")
(defun warn (mess &optional args)
  "Fake realgud:run-process used in testing"
  (setq last-mess mess)
  )

(setq last-mess nil)
(assert-equal '(("undodb-gdb" "-p") nil ("1955") nil)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb" "--interpreter=mi" "-p" "1955")))

(assert-nil (null last-mess))
(setq last-mess nil)

(assert-equal '(("undodb-gdb" "-p") nil ("1954") nil)
	      (realgud:undodb-gdb-parse-cmd-args
	       '("undodb-gdb" "-i" "mi" "-p" "1954")))

;; Restore the old value of realgud:run-process
(assert-nil (null last-mess))
(fset 'warn test:warn-save)


(end-tests)
