(setq elisp-file "../dbgr/common/core.el")
(require 'test-simple)
(load-file "../dbgr/common/lang.el")
(test-simple-start)

(note "dbgr-lang-mode?")

(assert-nil
 (dbgr-lang-mode? elisp-file "ruby")
 "dbgr-lang-mode? with Lisp file")


(save-excursion 
  (find-file "./gcd.rb")
  (assert-t
   (dbgr-lang-mode? "./gcd.rb" "ruby")
   "dbgr-lang-mode? with Ruby file")
  )


(assert-equal "gcd.rb"
	      (dbgr-suggest-lang-file "ruby" "\\.rb$")
	      "dbgr-suggest-lang-file"
	      )

(note "dbgr-suggest-file-from-buffer")


(with-current-buffer 
    (setq elisp-buffer (find-file "./test-dbgr.el"))
  (setq major-mode 'emacs-lisp-mode)
  (message "set major mode to %s" major-mode)
  )
(assert-equal (buffer-file-name elisp-buffer)
				(dbgr-suggest-file-from-buffer
				 "emacs-lisp"
				 (list elisp-buffer))
				"dbgr-lang-mode? with Lisp file"		  )

(end-tests)
