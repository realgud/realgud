;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/gub/core.el")

(declare-function __FILE__ 'load-relative)
(declare-function gub-parse-cmd-args 'realgud-gub-core)

(test-simple-start)

(note "form(s) involving gub.sh shell script")
(assert-equal '("gub.sh" ("--gub=\"-I\"") ("./gcd.go" "3" "5"))
	      (gub-parse-cmd-args
	       '("gub.sh" "--gub=\"-I\"" "--" "./gcd.go" "3" "5")))

(note "form(s) involving underlying tortoise interpreter")
(assert-equal '("tortoise" ("-run" "-gub=" "-interp=SS") ("./gcd.go" "3" "5"))
	      (gub-parse-cmd-args
	       '("tortoise" "-run" "-gub=" "-interp=SS" "--" "./gcd.go" "3" "5")))

(end-tests)
