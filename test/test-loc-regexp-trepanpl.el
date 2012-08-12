(load-file "./regexp-helper.el")
(load-file "../dbgr/debugger/trepan.pl/init.el")

(test-simple-start)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan.pl")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))


(setq text "-- main::(../example/gcd.pl:18)")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "../example/gcd.pl"
	      (match-string (dbgr-cmdbuf-info-file-group dbgr)
			    text))

(assert-equal "18"
	      (match-string 
	       (dbgr-cmdbuf-info-line-group dbgr)
	       text) "extract line number")


(end-tests)
