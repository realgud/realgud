(require 'test-unit)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-unit-clear-contexts)


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

(context "location matching"
	 (tag loc-regexp-trepan)

	 (specify "basic location"
		  (setq text "-- (/usr/local/bin/irb:9 @2)")
		  (assert-t (numberp (loc-match text)))
		  )
	 (specify "extract file name"
		  (assert-equal 0 (loc-match text))
		  (assert-equal "/usr/local/bin/irb"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
						text))
		  )
	 (specify "extract line number"
		  (assert-equal "9"
				(match-string 
				 (dbgr-cmdbuf-info-line-group dbgr)
				 text))
		  )

	 )


(context "location matching c-func"
	 (tag loc-regexp-trepan)

	 (specify "basic location for C fn"
		  (setq text "C> (/tmp/c-func.rb:2)")
		  (assert-t (numberp (loc-match text)))
		  )
	 (specify "extract file name for C fn"
		  (assert-equal 0 (loc-match text))
		  (assert-equal "/tmp/c-func.rb"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
						text))
		  )
	 (specify "extract line number for C fn"
		  (assert-equal "2"
				(match-string 
				 (dbgr-cmdbuf-info-line-group dbgr)
				 text))
		  )
	 )


(test-unit "loc-regexp-trepan")

