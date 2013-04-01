(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/trepanx/init.el")

(test-simple-start)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepanx")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq xagent-pat (gethash "rubinius-backtrace-Xagent" (gethash dbg-name realgud-pat-hash)))

(setq dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group  loc-pat)
		  :line-group (realgud-loc-pat-line-group  loc-pat)))


(defun xagent-match(text)
  (string-match (realgud-loc-pat-regexp xagent-pat) text)
)

(setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "basic location")

(note "extract file name")
(setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")
(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "../rbx-trepanning/tmp/rbxtest.rb"
	      (match-string (realgud-cmdbuf-info-file-group dbgr)
			    text))
(setq text "-- (../rbx-trepanning/tmp/rbxtest.rb:7 @5)")
(assert-equal "7"
	      (match-string
	       (realgud-cmdbuf-info-line-group dbgr)
	       text) "extract line number")

(setq text "0xbfb63710: RakeFileUtils#ruby in /home/rocky-rvm/.rvm/gems/rbx-head/gems/rake-0.8.7/lib/rake.rb:1094 (+61)")
(assert-t (numberp (xagent-match text)) "basic xagent location")

(end-tests)
