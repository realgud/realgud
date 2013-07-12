(require 'test-simple)
(load-file "../realgud/debugger/gub/core.el")

(test-simple-start)

(assert-equal '("gub.sh" ("--gub=\"-I\"") ("./gcd.go" "3" "5"))
	      (gub-parse-cmd-args
	       '("gub.sh" "--gub=\"-I\"" "--" "./gcd.go" "3" "5")))

(end-tests)
