;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/pdb/pdb.el")

(eval-when-compile (defvar test:run-process-save))

(declare-function pdb-parse-cmd-args 'realgud:pdb-core)
(declare-function realgud:pdb        'realgud:pdb)
(declare-function __FILE__           'load-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:run-process-save (symbol-function 'realgud:run-process))
(defun realgud:run-process(debugger-name script-filename cmd-args
				      minibuffer-histroy &optional no-reset)
  "Fake realgud:run-process used in testing"
  (note
   (format "%s %s %s" debugger-name script-filename cmd-args))
  (assert-equal "pdb" debugger-name "debugger name gets passed")
  (assert-equal (expand-file-name "./gcd.py") script-filename "file name check")
  (assert-equal '("3" "5") (cddr cmd-args) "command args listified")
  )

(note "pdb-parse-cmd-args")
(assert-equal (list nil '("pdb") (list (expand-file-name "foo")) nil)
	      (pdb-parse-cmd-args '("pdb" "foo")))
(assert-equal (list nil '("pdb") (list (expand-file-name "program.py") "foo") nil)
	      (pdb-parse-cmd-args
	       '("pdb" "program.py" "foo")))

(realgud:pdb "pdb ./gcd.py 3 5")
;; Restore the old value of realgud:run-process
(fset 'realgud:run-process test:run-process-save)

(end-tests)
