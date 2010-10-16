(setq elisp-file "../dbgr/common/core.el")
(require 'test-unit)
(load-file "../dbgr/common/lang.el")
(test-unit-clear-contexts)

(context "dbgr-lang-mode?"
	 (tag lang)
	 (specify "dbgr-lang-mode? with Lisp file"
		  (assert-nil
		   (dbgr-lang-mode? elisp-file "ruby")))

	 (specify "dbgr-lang-mode? with Ruby file"
		  (save-excursion (find-file "./gcd.rb"))
		  (assert-t
		   (dbgr-lang-mode? "./gcd.rb" "ruby")))
	 
	 (specify "dbgr-suggest-lang-file"
		  (assert-equal "gcd.rb"
				(dbgr-suggest-lang-file "ruby" "\\.rb$")))
	 )

(context "dbgr-suggest-file-from-buffer"
	 (tag lang)
	 (specify "dbgr-lang-mode? with Lisp file"
		  (with-current-buffer (setq elisp-buffer 
					(find-file "./test-dbgr.el"))
				  (setq major-mode 'emacs-lisp-mode)
				  (message "set major mode to %s" major-mode)
				  )
		  (assert-equal (buffer-file-name elisp-buffer)
				(dbgr-suggest-file-from-buffer
				 "emacs-lisp"
				 (list elisp-buffer)))
		  )
	 )


(test-unit "lang")
