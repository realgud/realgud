(require 'test-simple)
(load-file "../realgud/debugger/nodejs/nodejs.el")

(eval-when-compile (defvar test:run-process-save))

(declare-function nodejs-parse-cmd-args 'realgud:nodejs)
(declare-function nodejs                'realgud:nodejs)
(declare-function __FILE__              'require-relative)

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
  (assert-equal "node" debugger-name "debugger name gets passed")
  (let ((expanded-name (expand-file-name "./gcd.js")))
    (assert-equal  expanded-name script-filename "file name check")
    (assert-equal 'nodejs-track-mode track-mode-func)
    ))

(note "nodejs-parse-cmd-args")
(assert-equal (list '("node" "debug") nil '("foo"))
	      (nodejs-parse-cmd-args '("node" "debug" "foo")))

;; FIXME: need to mock remove-ansi-schmutz in realgud:nodejs
;; (realgud:nodejs "node debug ./gcd.js 3 5")

;; Restore the old value of realgud:run-process
(fset 'realgud:run-process test:run-process-save)

(end-tests)
