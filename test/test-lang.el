(eval-when-compile
  (defvar elisp-file)
  (defvar elisp-buffer)
)

(set (make-local-variable 'elisp-file)
     "../realgud/common/core.el")
(require 'test-simple)
(load-file "../realgud/common/lang.el")
(test-simple-start)

(declare-function realgud-suggest-lang-file 'realgud-lang)
(declare-function realgud-suggest-file-from-buffer 'realgud-lang)
(declare-function realgud-lang-mode? 'realgud-lang)

(note "realgud-lang-mode?")

(assert-nil
 (realgud-lang-mode? elisp-file "ruby")
 "realgud-lang-mode? with Lisp file")


(save-excursion
  (find-file "./gcd.rb")
  (assert-t
   (realgud-lang-mode? "./gcd.rb" "ruby")
   "realgud-lang-mode? with Ruby file")
  )


(assert-equal "gcd.rb"
              (realgud-suggest-lang-file "ruby" "\\.rb$")
              "realgud-suggest-lang-file"
              )

(assert-equal "default.bogus"
              (realgud-suggest-lang-file "bogus" "\\.bogus$" "default.bogus")
              "realgud-suggest-lang-file"
              )

(assert-t (file-exists-p (realgud-suggest-lang-file "bogus" "\\.bogus$")))

(note "realgud-suggest-file-from-buffer")


(with-current-buffer
    (setq elisp-buffer (find-file "./test-dbgr.el"))
  (set (make-local-variable 'major-mode)
       'emacs-lisp-mode)
  (message "set major mode to %s" major-mode)
  )
(assert-equal (buffer-file-name elisp-buffer)
                                (realgud-suggest-file-from-buffer
                                 "emacs-lisp"
                                 (list elisp-buffer))
                                "realgud-lang-mode? with Lisp file")

(end-tests)
