(require 'test-simple)
(load-file "../realgud/debugger/gub/core.el")

(declare-function __FILE__ 'require-relative)
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
