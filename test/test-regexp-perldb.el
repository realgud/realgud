(require 'test-unit)
(load-file "../dbgr/common/init/perldb.el")
(load-file "../dbgr/common/buffer/command.el")

(test-unit-clear-contexts)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "perldb")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))
(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group loc-pat)
		  :line-group (dbgr-loc-pat-line-group loc-pat)))


(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)

(setq text "main::(/usr/bin/latex2html:102):")
(context "perldb"
	 (tag perldb)

	 (specify "basic location"
		  (assert-t (numberp (loc-match text))))
	 (specify "extract file name"
		  (assert-equal "/usr/bin/latex2html"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
					      text)))
	 
	 (specify "extract line number"
		  (assert-equal "102"
				(match-string (dbgr-cmdbuf-info-line-group dbgr)
					      text)))

	 (specify "location for with CODE in it"
		  (setq text "main::CODE(0x9407ac8)(l2hconf.pm:6):")
		  (assert-t (numberp (loc-match text)))
		  (assert-equal "l2hconf.pm"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
					      text))
		  (assert-equal "6"
				(match-string (dbgr-cmdbuf-info-line-group dbgr)
					      text)))
	 )

(test-unit "perldb")

