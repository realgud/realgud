(require 'test-simple)
(load-file "../dbgr/debugger/rdebug/init.el")

(test-simple-start)

(setq tb  (gethash "lang-backtrace" dbgr-rdebug-pat-hash))
(setq bps (gethash "brkpt-set" dbgr-rdebug-pat-hash))
(setq rails-bt (gethash "rails-backtrace" dbgr-rdebug-pat-hash))

(defun loc-match(text var) 
  (string-match (dbgr-loc-pat-regexp var) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "	from /usr/local/bin/irb:12:in `<main>'")
(note "traceback location matching")
(lexical-let ((text "	from /usr/local/bin/irb:12:in `<main>'"))
  (assert-t (numberp (loc-match text tb)) "basic traceback location")
  (assert-equal "/usr/local/bin/irb"
		(match-string (dbgr-loc-pat-file-group tb)
			      text)
		"extract traceback file name")
  (assert-equal "12"
		(match-string (dbgr-loc-pat-line-group tb)
			      text) "extract traceback line number")
  )

(lexical-let ((text "Breakpoint 1 file /usr/bin/irb, line 10\n"))
  (assert-t (numberp (loc-match text bps)) "basic breakpoint location")
  (assert-equal "/usr/bin/irb"
		(match-string (dbgr-loc-pat-file-group bps)
			      text) "extract breakpoint file name")
  (assert-equal "10"
		(match-string (dbgr-loc-pat-line-group bps)
			      text)   "extract breakpoint line number")
  )

(end-tests)
