(require 'test-simple)
(load-file "../realgud/debugger/pdb/pdb.el")

(eval-when-compile (defvar test:run-process-save))

(declare-function pdb-parse-cmd-args 'realgud:pdb-core)
(declare-function realgud:pdb        'realgud:pdb)
(declare-function __FILE__           'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:run-process-save (symbol-function 'realgud:run-process))
(defun realgud:run-process(debugger-name script-filename cmd-args
				      track-mode-func minibuf-history
				      &optional no-reset)
  "Fake realgud:run-process used in testing"
  (note
   (format "%s %s %s %S" debugger-name script-filename cmd-args
	   track-mode-func))
  (assert-equal "pdb" debugger-name "debugger name gets passed")
  (assert-equal (expand-file-name "./gcd.py") script-filename "file name check")
  (assert-equal '("3" "5") (cddr cmd-args) "command args listified")
  (assert-equal 'pdb-track-mode-hook track-mode-func)
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
