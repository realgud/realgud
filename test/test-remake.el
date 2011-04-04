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

	 (specify "remake-file-suggest-priority"
	      (assert-equal 2 (remake-suggest-file-priority "foo"))
	      (let ((buffer (get-file-buffer "Makefile.am")))
	 	(if buffer (kill-buffer buffer))
	 	(assert-equal 2 (remake-suggest-file-priority "Makefile.am"))
	 	(setq buffer (find-file-noselect "Makefile.am"))
	 	(assert-equal 5 (remake-suggest-file-priority "Makefile.am"))
	 	(kill-buffer buffer)
	 	(setq buffer (get-file-buffer "Makefile"))
	 	(if buffer (kill-buffer buffer))
	 	(assert-equal 6 (remake-suggest-file-priority "Makefile"))
	 	(setq buffer (find-file-noselect "Makefile"))
	 	(assert-equal 8 (remake-suggest-file-priority "Makefile"))
	 	(kill-buffer buffer)
	      ))
	 )

(test-unit "remake")

