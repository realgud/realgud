;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/bashdb/bashdb.el")
(load-file "../realgud/debugger/bashdb/core.el")
(load-file "../realgud/common/core.el")
(load-file "../realgud/common/lang.el")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)

(eval-when-compile
  (defvar test:run-process-save)
  (defvar realgud:bashdb-minibuffer-history)
  )

(declare-function bashdb-parse-cmd-args 'realgud:bashdb)
(declare-function bashdb-suggest-invocation 'realgud:bzshdb)
(declare-function realgud:bashdb        'realgud:bashdb)
(declare-function __FILE__              'load-relative)

(test-simple-start)
(make-local-variable 'realgud:bashdb-minibuffer-history)
(setq realgud:bashdb-minibuffer-history nil)

;; Save value realgud:run-process and change it to something we want
(setq test:run-process-save (symbol-function 'realgud:run-process))
(defun realgud:run-process(debugger-name script-filename cmd-args
					 minibuf-history
					 &optional no-reset)
  "Fake realgud:run-process used in testing"
  (note
   (format "%s %s %s" debugger-name script-filename cmd-args))
  (assert-equal "bashdb" debugger-name "debugger name gets passed")
  (let ((expanded-name
	 (realgud:expand-file-name-if-exists "./gcd.sh")))
    (assert-equal  expanded-name script-filename "file name check")
    (assert-equal (list "-l" (expand-file-name ".") expanded-name "3" "5")
		  (cdr cmd-args) "command args listified")
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
(with-current-buffer (find-file "gcd.sh")
  (shell-script-mode)
  (assert-matches "bashdb .*gcd.sh$" (bashdb-suggest-invocation "bashdb")))

(realgud:bashdb "bashdb -l . ./gcd.sh 3 5")
;; Restore the old value of realgud:run-process
(fset 'realgud:run-process test:run-process-save)

(end-tests)
