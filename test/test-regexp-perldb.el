;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/lang/perl.el")
(load-file "../realgud/debugger/perldb/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)
(declare-function prompt-match          'regexp-helper)
(declare-function cmdbuf-loc-match      'realgud-regexp)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud:perldb-pat-hash)
  (defvar realgud-pat-hash)
  (defvar panic-tb)
  (defvar loc-pat)
  (defvar test-pos)
  (defvar prompt-pat)
  (defvar test-dbgr)
  (defvar carp-bt-re)
  (defvar file-group)
  (defvar line-group)
  (defvar test-text)
  (defvar lang-bt-pat)
  (defvar lang-bt-re)
  (defvar realgud-bt-pat)
  (defvar realgud-bt-re)
  (defvar test-s1)
  (defvar realgud-perl-ignore-file-re)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "perldb")
(set (make-local-variable 'loc-pat)
     (gethash "loc"    (gethash dbg-name realgud-pat-hash)))
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:perldb-pat-hash))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :alt-line-group (realgud-loc-pat-alt-line-group loc-pat)
		 :alt-file-group (realgud-loc-pat-alt-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)
		 :text-group (realgud-loc-pat-text-group loc-pat)
		 ))

(note "prompt")
(prompt-match "  DB<2> "  "2")
(prompt-match "[pid=6489->6502]  DB<1> " "1")

(setq test-text "((eval 1006)[../example/eval.pl:5])")
(assert-equal 1 (string-match realgud:perldb-loc-eval-regexp
			      test-text)
	      "perldb eval loc matching")

(assert-equal "../example/eval.pl"
	      (match-string (realgud-cmdbuf-info-alt-file-group test-dbgr)
			    test-text)
	      "extract file name when we have source text")

(assert-equal "5"
	      (match-string (realgud-cmdbuf-info-alt-line-group test-dbgr)
			    test-text))

(assert-equal 0 (string-match realgud-perl-ignore-file-re
			      "(eval 1006)[../example/eval.pl:5]")
	      "perldb file ignore matching")

(setq test-text "main::(/usr/bin/latex2html:102):\n")

(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
(assert-equal "/usr/bin/latex2html"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text)
	      "extract file name")

(setq test-text "File::Basename::dirname(/usr/share/perl/5.16.0/File/Basename.pm:284):	my $path = shift;
")

(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr))
	  "location with source")
(assert-equal "/usr/share/perl/5.16.0/File/Basename.pm"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text)
	      "extract file name when we have source text")
(assert-equal "284"
	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
			    test-text)
	      "extract line number when we have source text")
(assert-equal "my $path = shift;"
	      (match-string (realgud-cmdbuf-info-text-group test-dbgr)
			    test-text)
	      "extract source text")

(setq test-text "main::((eval 6)[eval.pl:5]:2):	$x = 2;")

(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "eval location")

(assert-equal "eval.pl"
	      (match-string (realgud-cmdbuf-info-alt-file-group test-dbgr)
			    test-text)
	      "extract eval file name")

(assert-equal "5"
	      (match-string (realgud-cmdbuf-info-alt-line-group test-dbgr)
			    test-text) "extract line number")

(note "location for with CODE in it")
(setq test-text "main::CODE(0x9407ac8)(l2hconf.pm:6):\n")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)))
(assert-equal "l2hconf.pm"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text))
(assert-equal "6"
	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
			    test-text))

(note "debugger-backtrace")
(setq realgud-bt-pat  (gethash "debugger-backtrace"
			    realgud:perldb-pat-hash))
(setq test-s1
      "$ = main::top_navigation_panel called from file `./latex2html' line 7400
p")
(setq realgud-bt-re (realgud-loc-pat-regexp realgud-bt-pat))
(setq file-group (realgud-loc-pat-file-group realgud-bt-pat))
(setq line-group (realgud-loc-pat-line-group realgud-bt-pat))
(assert-equal 30 (string-match realgud-bt-re test-s1))
(assert-equal "./latex2html"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "7400"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(note "debugger-errmsg")
(setq realgud-bt-pat  (gethash "perl-errmsg"
			    realgud:perldb-pat-hash))
(setq test-s1
      "Use of uninitialized value $lines[0] in join or string at bin/../lib/LineCache.pm line 548.")
(setq realgud-bt-re (realgud-loc-pat-regexp realgud-bt-pat))
(setq file-group (realgud-loc-pat-file-group realgud-bt-pat))
(setq line-group (realgud-loc-pat-line-group realgud-bt-pat))
(assert-equal 54 (string-match realgud-bt-re test-s1))
(assert-equal "bin/../lib/LineCache.pm"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "548"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(note "carp-backtrace")
(setq test-s1
      " at /tmp/foo.pl line 7
 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at foo2.pl line 5
 	main::foo(3) called at foo3.pl line 8
")
(setq lang-bt-pat (gethash "lang-backtrace"
			   realgud:perldb-pat-hash))
(setq carp-bt-re (realgud-loc-pat-regexp lang-bt-pat))
(setq file-group (realgud-loc-pat-file-group lang-bt-pat))
(setq line-group (realgud-loc-pat-line-group lang-bt-pat))
(assert-equal 0 (string-match carp-bt-re test-s1))
(assert-equal "/tmp/foo.pl"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "7"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))

(assert-equal 22 (string-match carp-bt-re test-s1 test-pos))
(assert-equal "foo2.pl"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "5"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 119 (string-match carp-bt-re test-s1 test-pos))
(assert-equal "foo3.pl"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "8"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(end-tests)
