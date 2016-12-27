;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/gub/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(declare-function __FILE__                       'load-relative)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud:gub-pat-hash)
  (defvar panic-tb)
  (defvar test-tb)
  (defvar prompt-pat)
  (defvar test-dbgr)
  (defvar test-text)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "gub")

(note "gub prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:gub-pat-hash))
(prompt-match "gub[0]: ")
(prompt-match "gub[20]: ")
(prompt-match "gub[32@5]: ")

(setup-regexp-vars realgud:gub-pat-hash)
(set (make-local-variable 'test-tb)
     (gethash "lang-backtrace"  realgud:gub-pat-hash))

(note "go lang traceback")
(setq test-text "/usr/local/go/src/pkg/runtime/panic.c:482 (0x805c956)")

(assert-t (numberp (tb-loc-match test-text)) "go traceback location")
(assert-equal "/usr/local/go/src/pkg/runtime/panic.c"
	      (match-string (realgud-loc-pat-file-group test-tb)
			    test-text) "extract traceback file name")
(assert-equal "482"
	      (match-string (realgud-loc-pat-line-group test-tb)
			    test-text)   "extract traceback line number")

(note "panic traceback")
(setq test-text "	/tmp/github.com/rocky/ssa-interp/eval/selectorexpr.go:18 +0x9f")

(set (make-local-variable 'panic-tb)
     (gethash "panic-backtrace"  realgud:gub-pat-hash))

(assert-t (numberp (string-match (realgud-loc-pat-regexp panic-tb) test-text))
	  "go panic location")
(assert-equal "/tmp/github.com/rocky/ssa-interp/eval/selectorexpr.go"
	      (match-string (realgud-loc-pat-file-group test-tb)
			    test-text) "extract panic traceback file name")
(assert-equal "18"
	      (match-string (realgud-loc-pat-line-group test-tb)
			    test-text)   "extract panic traceback line number")

(end-tests)
