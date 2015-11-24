;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

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
(declare-function realgud-loc-pat-num            'realgud-regexp)
(declare-function test-simple-start 'test-simple)
(declare-function assert-t 'test-simple)
(declare-function assert-equal 'test-simple)
(declare-function note 'test-simple)
(declare-function end-tests 'test-simple)

(test-simple-start)

(eval-when-compile
  (defvar file-group)
  (defvar frame-re)
  (defvar line-group)
  (defvar num-group)
  (defvar test-pos)
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar loc-pat)
  (defvar test-dbgr)
  (defvar test-s1)
  (defvar realgud-pat-bt)
  (defvar realgud:trepanpl-pat-hash)
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


(setq test-s1 "-- main::(../example/gcd.pl:18)")
(assert-t (numberp (cmdbuf-loc-match test-s1 test-dbgr)) "basic location")

(assert-equal 0 (cmdbuf-loc-match test-s1 test-dbgr))

(note "extract location fields")
(assert-equal "../example/gcd.pl"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-s1))

(assert-equal "18"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-s1) "extract line number")


(note "Test with hex location")
(setq test-s1 "-- File::Basename::(/usr/share/perl/5.14/File/Basename.pm:284 @0x8918b70)")
(assert-t (numberp (cmdbuf-loc-match test-s1 test-dbgr)) "basic location")
(assert-equal 0 (cmdbuf-loc-match test-s1 test-dbgr))

(assert-equal "/usr/share/perl/5.14/File/Basename.pm"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-s1))

(assert-equal "284"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-s1) "extract line number")


(note "debugger-backtrace")
(setq realgud-pat-bt  (gethash "debugger-backtrace"
			     realgud:trepanpl-pat-hash))
(setq test-s1
      "--> #0 @ = File::Basename::fileparse('/usr/local/bin/trepan.pl') in
	file `/usr/share/perl/5.18.2/File/Basename.pm' at line 107
    #1 @ = File::Basename::dirname('/usr/local/bin/trepan.pl') in
	file `/usr/share/perl/5.18.2/File/Basename1.pm' at line 294
    #2 file `/usr/local/bin/trepan.pl' at line 11
")
(setq frame-re (realgud-loc-pat-regexp realgud-pat-bt))
(setq num-group (realgud-loc-pat-num realgud-pat-bt))
(setq file-group (realgud-loc-pat-file-group realgud-pat-bt))
(setq line-group (realgud-loc-pat-line-group realgud-pat-bt))
(assert-equal 0 (string-match frame-re test-s1))
(assert-equal "0" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/usr/share/perl/5.18.2/File/Basename.pm"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "107"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))

(assert-equal 127 (string-match frame-re test-s1 test-pos))
(assert-equal "1" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/usr/share/perl/5.18.2/File/Basename1.pm"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "294"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))
(assert-equal 254 test-pos)

(end-tests)
