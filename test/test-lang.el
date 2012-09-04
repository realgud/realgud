(set (make-local-variable 'elisp-file)
     "../dbgr/common/core.el")
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

(assert-equal "default.bogus"
              (dbgr-suggest-lang-file "bogus" "\\.bogus$" "default.bogus")
              "dbgr-suggest-lang-file"
              )

(assert-t (file-exists-p (dbgr-suggest-lang-file "bogus" "\\.bogus$")))

(note "dbgr-suggest-file-from-buffer")


(with-current-buffer 
    (setq elisp-buffer (find-file "./test-dbgr.el"))
  (set (make-local-variable 'major-mode)
       'emacs-lisp-mode)
  (message "set major mode to %s" major-mode)
  )
(assert-equal (buffer-file-name elisp-buffer)
                                (dbgr-suggest-file-from-buffer
                                 "emacs-lisp"
                                 (list elisp-buffer))
                                "dbgr-lang-mode? with Lisp file"                  )

(end-tests)
