(load-file "./behave.el")
(load-file "../dbgr-regexp.el")
(load-file "../dbgr-procbuf-var.el")

(behave-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger rbdbgr. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "rbdbgr")
(setq loc-pat (gethash dbg-name dbgr-pat-hash))

(setq dbgr (make-dbgr-info
		  :name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))


(defun loc-match(text) 
  (string-match (dbgr-info-loc-regexp dbgr) text)
)

(context "location matching: "
	 (tag regexp)
	 (lexical-let ((text ".. (./dbgrr.rb:73)")
		       (text2 "C> ((eval):1 via /tmp/eval2.rb:2)")
		       (text3 "-- (<internal:prelude>:28 remapped prelude.rb:28)")
		       (text4 "-- (/src/external-vcs/dbgrr/processor/command/info_subcmd/registers_subcmd/dfp.rb:2)\nrequire_relative %w(.. .. base subsubcmd)\n")
		       )
	   (specify "basic location"
		    (expect (numberp (loc-match text)) t))
	   (specify "extract file name"
	   	    (expect (match-string (dbgr-info-file-group dbgr)
	   				  text)
	   		    equal "./dbgrr.rb"))
	   (specify "extract line number"
	   	    (expect (match-string (dbgr-info-line-group dbgr)
	   				  text)
	   		    equal "73"))
	   (specify "more compex location"
		    (expect (numberp (loc-match text4)) t))


	   ;; Now try via
	   (specify "basic via location"
	   	    (expect (numberp (loc-match text2)) t))
	   (specify "extract via file name"
	   	    (expect (match-string (dbgr-info-file-group dbgr)
	   				  text2)
	   		    equal "/tmp/eval2.rb"))
	   (specify "extract via line number"
	   	    (expect (match-string (dbgr-info-line-group dbgr)
	   				  text2)
	   		    equal "2"))

	   ;; Now try remap
	   (specify "basic via location"
	   	    (expect (numberp (loc-match text3)) t))

	   ;;
	   (specify "unmatched location"
	   	    (setq text "--> #0 METHOD Object#square(x) in file ./rbdbgr.rb at line 73")
	   	    (expect (numberp (loc-match text)) equal nil))
	   
	   ))

(behave "regexp")

