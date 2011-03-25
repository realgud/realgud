(require 'test-unit)
(load-file "../dbgr/debugger/remake/core.el")

(test-unit-clear-contexts)

(context "remake"
	 (tag remake)

	 (specify "remake-parse-cmd-args"
	      (assert-equal '("remake" "Makefile" ("-X" "-f" "Makefile"))
			    (remake-parse-cmd-args 
			     '("remake" "-X" "-f" "Makefile")))
	      )
	 (specify "remake-suggest-Makefile"
	      (assert-equal "Makefile" (remake-suggest-Makefile))
	      )
	 )

(test-unit "remake")

