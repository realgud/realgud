(require 'test-unit)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepanx/init.el")

(test-unit-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepanx")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))


(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)

(context "location matching"
	 (tag loc-regexp-trepanx)
	 (setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")

	 (specify "basic location"
		  (assert-t (numberp (loc-match text)))
		  )
	 (specify "extract file name"
		  (assert-equal 0 (loc-match text))
		  (assert-equal "../rbx-trepanning/tmp/rbxtest.rb"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
						text))
		  )
	   (specify "extract line number"
	   	    (assert-equal "7"
				  (match-string 
				   (dbgr-cmdbuf-info-line-group dbgr)
				   text))
		    )
	   )

(test-unit "loc-regexp-trepanx")

