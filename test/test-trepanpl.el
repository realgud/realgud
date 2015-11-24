;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/trepan.pl/trepanpl.el")

(eval-when-compile (defvar test:run-process-save))

(declare-function realgud:trepanpl-parse-cmd-args 'realgud:trepanpl)
(declare-function realgud:trepan.pl               'realgud:trepanpl)
(declare-function __FILE__                        'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:run-process-save (symbol-function 'realgud:run-process))
(defun realgud:run-process(debugger-name script-filename cmd-args
				      minibuf-history
				      &optional no-reset)
  "Fake realgud:run-process used in testing"
  (note
   (format "%s %s %s" debugger-name script-filename cmd-args))
  (assert-equal "trepan.pl" debugger-name "debugger name gets passed")
  (let ((expanded-name (expand-file-name "./gcd.pl")))
    (assert-equal  expanded-name script-filename "file name check")
    (assert-equal (list "-I" (expand-file-name ".") expanded-name "3" "5")
		  (cdr cmd-args) "command args listified")
    )
  nil ;; Make sure trepanpl doesn't try to do anything with cmdbuf
  )

(note "realgud:trepanpl-parse-cmd-args")
(assert-equal (list nil '("trepan.pl") '("foo"))
	      (realgud:trepanpl-parse-cmd-args '("trepan.pl" "foo")))
(assert-equal (list '("perl5.8") '("trepan.pl") '("foo"))
	      (realgud:trepanpl-parse-cmd-args '("perl5.8" "trepan.pl" "foo")))
(assert-equal (list nil '("trepan.pl") '("program.pl" "foo"))
	      (realgud:trepanpl-parse-cmd-args
	       '("trepan.pl" "program.pl" "foo")))
(assert-equal (list nil '("trepan.pl") (list (expand-file-name "gcd.pl") "foo"))
	      (realgud:trepanpl-parse-cmd-args
	       '("trepan.pl" "gcd.pl" "foo")))

(realgud:trepan.pl "trepanpl -I . ./gcd.pl 3 5")

;; Restore the old value of realgud:run-process
(fset 'realgud:run-process test:run-process-save)

(end-tests)
