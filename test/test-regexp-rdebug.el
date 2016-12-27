;; -*- lexical-binding:t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/rdebug/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(eval-when-compile
  (defvar helper-bps)
  (defvar helper-tb)
  (defvar prompt-pat)
  (defvar rails-bt)
  (defvar realgud-rdebug-pat-hash)
  (defvar test-text)
)

(setup-regexp-vars realgud-rdebug-pat-hash)
(setq rails-bt (gethash "rails-backtrace" realgud-rdebug-pat-hash))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq test-text "	from /usr/local/bin/irb:12:in `<main>'")
(note "traceback location matching")
(let ((text "	from /usr/local/bin/irb:12:in `<main>'"))
  (assert-t (numberp (loc-match text helper-tb)) "basic traceback location")
  (assert-equal "/usr/local/bin/irb"
		(match-string (realgud-loc-pat-file-group helper-tb)
			      text)
		"extract traceback file name")
  (assert-equal "12"
		(match-string (realgud-loc-pat-line-group helper-tb)
			      text) "extract traceback line number")
  )

(let ((text "Breakpoint 1 file /usr/bin/irb, line 10\n"))
  (assert-t (numberp (loc-match text helper-bps)) "basic breakpoint location")
  (assert-equal "/usr/bin/irb"
		(match-string (realgud-loc-pat-file-group helper-bps)
			      text) "extract breakpoint file name")
  (assert-equal "10"
		(match-string (realgud-loc-pat-line-group helper-bps)
			      text)   "extract breakpoint line number")
  )

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud-rdebug-pat-hash))
(prompt-match "(rdb:1) ")

(end-tests)
