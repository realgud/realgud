;; -*- lexical-binding:t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

;; Don't have a pat-hash for ruby, so we need something that pulls in
;; Ruby.
(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/rdebug/init.el")

(test-simple-start)

(setq bt  (gethash "rails-backtrace" realgud-rdebug-pat-hash))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "/tmp/rails-2.3.5/lib/tasks/databases.rake:360")

(let ((text "/tmp/rails-2.3.5/lib/tasks/databases.rake:360"))

  (assert-t (numberp (loc-match text bt)) "basic traceback location")
  (assert-equal "/tmp/rails-2.3.5/lib/tasks/databases.rake"
		(match-string (realgud-loc-pat-file-group bt)
			      text) "extract traceback file name")
  (assert-equal "360"
		(match-string (realgud-loc-pat-line-group bt)
			      text)   "extract traceback line number")
  )

(let ((text
       "/tmp/gems/rake-0.8.7/lib/rake.rb:597:in `invoke_with_call_chain'"))

  (assert-t (numberp (loc-match text bt)) "traceback location with in")
  (assert-equal "/tmp/gems/rake-0.8.7/lib/rake.rb"
		(match-string (realgud-loc-pat-file-group bt)
			      text)   "extract traceback file name 2")
  (assert-equal "597"
		(match-string (realgud-loc-pat-line-group bt)
			      text)   "extract breakpoint line number 2")
  )

(end-tests)
