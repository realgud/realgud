(load-file "./regexp-helper.el")
(load-file "../realgud/common/regexp.el")
(load-file "../realgud/debugger/trepan.pl/init.el")

(declare-function realgud-cmdbuf-info-loc-regexp 'realgud-buffer-command)
(declare-function cmdbuf-loc-match               'realgud-regexp-helper)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)
(declare-function realgud-cmdbuf-info-file-group 'realgud-regexp)
(declare-function realgud-cmdbuf-info-line-group 'realgud-regexp)
(declare-function realgud-cmdbuf-info            'realgud-regexp)
(declare-function make-realgud-cmdbuf-info       'realgud-regexp)
(declare-function test-simple-start 'test-simple)
(declare-function assert-t 'test-simple)
(declare-function assert-equal 'test-simple)
(declare-function note 'test-simple)
(declare-function end-tests 'test-simple)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar loc-pat)
  (defvar test-dbgr)
  (defvar test-text)
)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan.pl")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))


(setq test-text "-- main::(../example/gcd.pl:18)")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")

(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))

(note "extract location fields")
(assert-equal "../example/gcd.pl"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text))

(assert-equal "18"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number")


(note "Test with hex location")
(setq test-text "-- File::Basename::(/usr/share/perl/5.14/File/Basename.pm:284 @0x8918b70)")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))

(assert-equal "/usr/share/perl/5.14/File/Basename.pm"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text))

(assert-equal "284"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number")

(end-tests)
