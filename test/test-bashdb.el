(require 'test-simple)
(load-file "../realgud/debugger/bashdb/bashdb.el")
(load-file "../realgud/common/core.el")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)

(eval-when-compile (defvar test:run-process-save))

(declare-function bashdb-parse-cmd-args 'realgud:bashdb)
(declare-function realgud:bashdb        'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:run-process-save (symbol-function 'realgud:run-process))
(defun realgud:run-process(debugger-name script-filename cmd-args
					 track-mode-func mini-history
					 &optional no-reset)
  "Fake realgud:run-process used in testing"
  (note
   (format "%s %s %s %S" debugger-name script-filename cmd-args
	   track-mode-func))
  (assert-equal "bashdb" debugger-name "debugger name gets passed")
  (let ((expanded-name
	 (realgud:expand-file-name-if-exists "./gcd.sh")))
    (assert-equal  expanded-name script-filename "file name check")
    (assert-equal (list "-l" (expand-file-name ".") expanded-name "3" "5")
		  (cdr cmd-args) "command args listified")
    (assert-equal 'bashdb-track-mode-hook track-mode-func)
    ))

(note "bashdb-parse-cmd-args")
(assert-equal (list nil '("bashdb")
		    (list (realgud:expand-file-name-if-exists "foo")) nil)
	      (bashdb-parse-cmd-args '("bashdb" "foo")))
(assert-equal (list nil '("bashdb")
		    (list (realgud:expand-file-name-if-exists "program.sh")
			  "foo") nil)
	      (bashdb-parse-cmd-args
	       '("bashdb" "program.sh" "foo")))

(realgud:bashdb "bashdb -l . ./gcd.sh 3 5")
;; Restore the old value of realgud:run-process
(fset 'realgud:run-process test:run-process-save)

(end-tests)
