;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)

(load-file "../realgud/lang/js.el")
(test-simple-start)

(setq test-text "/src/external-vcs/github/rocky/trepan-ni/lib/internal/inspect_repl.js:637")
(assert-t (string-match (realgud-loc-pat-regexp realgud:js-file-line-loc-pat)
			test-text) "basic location")
(assert-equal "/src/external-vcs/github/rocky/trepan-ni/lib/internal/inspect_repl.js"
	      (match-string 1 test-text)   "extract file name")
(assert-equal "637"
	      (match-string 2 test-text)   "extract line number")


(end-tests)
