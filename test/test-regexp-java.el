;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/lang/java.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)
(declare-function prompt-match          'regexp-helper)

(test-simple-start)

(eval-when-compile
  (defvar loc-pat)   (defvar realgud:flake8-msg-loc-pat)
  (defvar test-text)
  )

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(note "maven testing")

(setq test-text
      "[ERROR] /Users/rocky/pymaven/LexumoIndexer.java:[203,26] error: ';' expected")

(assert-t (numberp (loc-match test-text realgud-maven-error-loc-pat))
	  "maven error")

(assert-equal "/Users/rocky/pymaven/LexumoIndexer.java"
	      (match-string (realgud-loc-pat-file-group realgud-maven-error-loc-pat)
			    test-text))

(assert-equal "203"
	      (match-string (realgud-loc-pat-line-group realgud-maven-error-loc-pat)
			    test-text))

(assert-equal "26"
	      (match-string (realgud-loc-pat-char-offset-group realgud-maven-error-loc-pat)
			    test-text))

(end-tests)
