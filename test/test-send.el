(load-file "./behave.el")

;; FIXME: not sure why we have to include since dbgr-send includes this.
(load-file "../dbgr-buffer.el")

(load-file "../dbgr-send.el")
(behave-clear-contexts)

(context "dbgr-send"
	 (tag send)
	 (specify "format no expand characters"
		  (dolist (str '("abc" "100%" "I feel %% today"))
		    (assert-equal str (dbgr-expand-format str)))
	   ))

(behave "send")

