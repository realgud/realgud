(require 'test-simple)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-simple-start)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))


(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)

(lexical-let ((text ".. (./dbgr.rb:73)")
	      (text2 "C> ((eval):1 via /tmp/eval2.rb:2)")
	      (text3 "-- (<internal:prelude>:28 remapped prelude.rb:28)")
	      (text4 "-- (/src/external-vcs/dbgrr/processor/command/info_subcmd/registers_subcmd/dfp.rb:2)\nrequire_relative %w(.. .. base subsubcmd)\n")
	      )
  
  (assert-t (numberp (loc-match text)) "basic location")
  (assert-equal "./dbgr.rb"
		(match-string (dbgr-cmdbuf-info-file-group dbgr)
			      text)   "extract file name")
  (assert-equal "73"
		(match-string (dbgr-cmdbuf-info-line-group dbgr)
			      text)   "extract line number")
  (assert-t (numberp (loc-match text4))   "more complex location")
  
  
  ;; Now try 'via'
  (assert-t (numberp (loc-match text2)) "basic 'via' location")
  (assert-equal "/tmp/eval2.rb"
		(match-string (dbgr-cmdbuf-info-file-group dbgr)
			      text2)
		  "extract via file name")
  (assert-equal "2" (match-string (dbgr-cmdbuf-info-line-group dbgr)
				  text2)
		"extract via line number")
  
  ;; Now try remap
  (assert-t (numberp (loc-match text3)) "basic 'via' location")
  
  ;;
  (setq text "--> #0 METHOD Object#square(x) in file ./trepan.rb at line 73")
  (assert-nil (numberp (loc-match text)) "unmatched location")
  
  )

(end-tests)
