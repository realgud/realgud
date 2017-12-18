;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/remake/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(eval-when-compile
  (defvar prompt-pat) (defvar frame-pat)   (defvar frame-re)
  (defvar loc-pat)    (defvar prompt-pat)  (defvar test-text)
  (defvar file-group) (defvar line-group)  (defvar test-pos)
  (defvar num-pat)    (defvar num-group)   (defvar realgud:remake-pat-hash)
)

(declare-function prompt-match                   'regexp-helper)
(declare-function realgud-loc-pat-num            'realgud-regexp)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)

(set (make-local-variable 'prompt-pat)
     (gethash "prompt"             realgud:remake-pat-hash))
(set (make-local-variable 'frame-pat)
     (gethash "debugger-backtrace" realgud:remake-pat-hash))

(note "remake prompt")
(prompt-match "remake<10> "  "10")
(prompt-match	"remake<<1>> " "1" "recursive remake %s")

(note "remake debugger-backtrace")
(setq test-text
      "=>#0  Makefile.in at /tmp/Makefile:216
  #1  Makefile at /tmp/Makefile:230
")

(set (make-local-variable 'frame-re)
     (realgud-loc-pat-regexp frame-pat))
(set (make-local-variable 'num-group)
     (realgud-loc-pat-num frame-pat))
(set (make-local-variable 'file-group)
     (realgud-loc-pat-file-group frame-pat))
(set (make-local-variable 'line-group)
     (realgud-loc-pat-line-group frame-pat))

(assert-equal 0 (string-match frame-re test-text))
(assert-equal "0" (substring test-text
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/tmp/Makefile"
	      (substring test-text
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "216"
	      (substring test-text
			 (match-beginning line-group)
			 (match-end line-group)))
(set (make-local-variable 'test-pos)
     (match-end 0))

(assert-equal 39 (string-match frame-re test-text test-pos))
(assert-equal "1" (substring test-text
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/tmp/Makefile"
	      (substring test-text
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "230"
	      (substring test-text
			 (match-beginning line-group)
			 (match-end line-group)))

(end-tests)
