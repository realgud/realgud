;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/remake/core.el")

(declare-function __FILE__ 'load-relative)
(declare-function remake-parse-cmd-args        'realgud-remake-core)
(declare-function remake-suggest-Makefile      'realgud-remake-core)
(declare-function remake-suggest-file-priority 'realgud-remake-core)

(test-simple-start)

(assert-equal (list "remake" (expand-file-name "Makefile")
		    (list "-X" "-f" (expand-file-name "Makefile")))
	      (remake-parse-cmd-args
	       '("remake" "-X" "-f" "Makefile"))
	      "remake-parse-cmd-args")

(assert-equal "Makefile" (remake-suggest-Makefile) "remake-suggest-Makefile")

(assert-equal 2 (remake-suggest-file-priority "foo")
	      "remake-file-suggest-priority")
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
  )

(end-tests)
