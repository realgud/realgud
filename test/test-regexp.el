(require 'test-unit)
(load-file "../dbgr/common/init.el")
(load-file "../dbgr/common/cmdbuf.el")

(test-unit-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger rbdbgr. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "rbdbgr")
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
	 (tag regexp)
	 (lexical-let ((text ".. (./dbgr.rb:73)")
		       (text2 "C> ((eval):1 via /tmp/eval2.rb:2)")
		       (text3 "-- (<internal:prelude>:28 remapped prelude.rb:28)")
		       (text4 "-- (/src/external-vcs/dbgrr/processor/command/info_subcmd/registers_subcmd/dfp.rb:2)\nrequire_relative %w(.. .. base subsubcmd)\n")
		       )
	   (specify "basic location"
		    (assert-t (numberp (loc-match text))))
	   (specify "extract file name"
	   	    (assert-equal "./dbgr.rb"
				  (match-string (dbgr-cmdbuf-info-file-group dbgr)
						text)))
	   (specify "extract line number"
	   	    (assert-equal "73"
				  (match-string (dbgr-cmdbuf-info-line-group dbgr)
						text)))
	   (specify "more complex location"
		    (assert-t (numberp (loc-match text4))))


	   ;; Now try via
	   (specify "basic via location"
	   	    (assert-t (numberp (loc-match text2))))
	   (specify "extract via file name"
	   	    (assert-equal "/tmp/eval2.rb"
				  (match-string (dbgr-cmdbuf-info-file-group dbgr)
						text2)
				  ))
	   (specify "extract via line number"
	   	    (assert-equal (match-string (dbgr-cmdbuf-info-line-group dbgr)
						text2)
	   		    "2"))

	   ;; Now try remap
	   (specify "basic via location"
	   	    (assert-t (numberp (loc-match text3))))

	   ;;
	   (specify "unmatched location"
	   	    (setq text "--> #0 METHOD Object#square(x) in file ./rbdbgr.rb at line 73")
	   	    (assert-nil (numberp (loc-match text))))
	   
	   ))

(test-unit "regexp")

