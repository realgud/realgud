;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/lang/python.el")
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
(note "flake8 testing")

(setq test-text
      "uncompyle6/parsers/parse3.py:441:17: W503 line break before binary operator")

(assert-t (numberp (loc-match test-text realgud-flake8-msg-loc-pat))
	  "flake8 warning")

(assert-equal "uncompyle6/parsers/parse3.py"
	      (match-string (realgud-loc-pat-file-group realgud-flake8-msg-loc-pat)
			    test-text))

(assert-equal "441"
	      (match-string (realgud-loc-pat-line-group realgud-flake8-msg-loc-pat)
			    test-text))

(assert-equal "17"
	      (match-string (realgud-loc-pat-char-offset-group realgud-flake8-msg-loc-pat)
			    test-text))

(setq test-text
      "/uncompyle6/main.py:53:1: E303 too many blank lines (3)")


(assert-t (numberp (loc-match test-text realgud-flake8-msg-loc-pat))
	  "flake8 error")

(assert-equal "/uncompyle6/main.py"
	      (match-string (realgud-loc-pat-file-group realgud-flake8-msg-loc-pat)
			    test-text))

(assert-equal "53"
	      (match-string (realgud-loc-pat-line-group realgud-flake8-msg-loc-pat)
			    test-text))

(assert-equal "1"
	      (match-string (realgud-loc-pat-char-offset-group realgud-flake8-msg-loc-pat)
			    test-text))

;; FIXME add pytest testing
;; (note "pytest testing")


(end-tests)
