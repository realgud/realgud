(require 'test-unit)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepanx/init.el")

(test-unit-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepanx")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))
(setq xagent-pat (gethash "rubinius-backtrace-Xagent" (gethash dbg-name dbgr-pat-hash)))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))


(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)

(defun xagent-match(text) 
  (string-match (dbgr-loc-pat-regexp xagent-pat) text)
)

(context "trepanx location matching"
	 (tag loc-regexp-trepanx)

	 (specify "basic location"
		  (setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")
		  (assert-t (numberp (loc-match text)))
		  )
	 (specify "extract file name"
		  (setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")
		  (assert-equal 0 (loc-match text))
		  (assert-equal "../rbx-trepanning/tmp/rbxtest.rb"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
						text))
		  )
	   (specify "extract line number"
		    (setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")
	   	    (assert-equal "7"
				  (match-string 
				   (dbgr-cmdbuf-info-line-group dbgr)
				   text))
		    )

	 (specify "basic xagent location"
		  (setq text "0xbfb63710: RakeFileUtils#ruby in /home/rocky-rvm/.rvm/gems/rbx-head/gems/rake-0.8.7/lib/rake.rb:1094 (+61)")
		  (assert-t (numberp (xagent-match text)))
		  )

	   )


(test-unit "loc-regexp-trepanx")

