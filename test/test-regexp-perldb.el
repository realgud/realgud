(require 'test-unit)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/perldb/init.el")

(test-unit-clear-contexts)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "perldb")
(setq loc-pat     (gethash "loc"    (gethash dbg-name dbgr-pat-hash)))
(setq prompt-pat  (gethash "prompt" dbgr-perldb-pat-hash))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group loc-pat)
		  :line-group (dbgr-loc-pat-line-group loc-pat)))


(defun prompt-match(prompt-str num-str) 
  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt-pat)
				prompt-str))
)

(context "perldb prompt matching"
	 (tag regexp-perldb)
	 (specify "prompt"
		  (prompt-match "  DB<2> "  "2")
		  (prompt-match	"[pid=6489->6502]  DB<1> " "1")
		  )
	 )

(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)

(setq text "main::(/usr/bin/latex2html:102):")
(context "perldb"
	 (tag regexp-perldb)

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

	 (specify "debugger-backtrace"
		  (setq dbg-bt-pat  (gethash "debugger-backtrace"  
					     dbgr-perldb-pat-hash))
		  (setq s1
			"$ = main::top_navigation_panel called from file `./latex2html' line 7400
")
		  (setq dbgr-bt-re (dbgr-loc-pat-regexp dbg-bt-pat))
		  (setq file-group (dbgr-loc-pat-file-group dbg-bt-pat))
		  (setq line-group (dbgr-loc-pat-line-group dbg-bt-pat))
		  (assert-equal 30 (string-match dbgr-bt-re s1))
		  (assert-equal "./latex2html"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "7400"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  )

	 (specify "carp-backtrace"
		  (setq s1
			" at /tmp/foo.pl line 7
 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at foo2.pl line 5
 	main::foo(3) called at foo3.pl line 8
")
		  (setq lang-bt-pat (gethash "lang-backtrace"  
					     dbgr-perldb-pat-hash))
		  (setq carp-bt-re (dbgr-loc-pat-regexp lang-bt-pat))
		  (setq file-group (dbgr-loc-pat-file-group lang-bt-pat))
		  (setq line-group (dbgr-loc-pat-line-group lang-bt-pat))
		  (assert-equal 0 (string-match carp-bt-re s1))
		  (assert-equal "/tmp/foo.pl"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "7"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  (setq pos (match-end 0))
		  
		  (assert-equal 22 (string-match carp-bt-re s1 pos))
		  (assert-equal "foo2.pl"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "5"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  
		  (setq pos (match-end 0))
		  (assert-equal 119 (string-match carp-bt-re s1 pos))
		  (assert-equal "foo3.pl"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "8"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  )

	 )

(test-unit "regexp-perldb")

