;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/rdebug/rdebug.el")
(declare-function rdebug-get-script-name 'realgud-rdebug)
(declare-function __FILE__               'load-relative)
(test-simple-start)

(note "rdebug-get-script-name")
(assert-equal '("foo" nil)
	      (rdebug-get-script-name '("rdebug" "foo")))
(assert-equal '("foo" nil)
	      (rdebug-get-script-name '("rdebug" "-m" "foo")))
(assert-equal '("foo" t)
	      (rdebug-get-script-name
	       '("rdebug" "--emacs" "3" "foo")))
(assert-equal '("foo" t)
	      (rdebug-get-script-name
	       '("myrdebug" "--annotate=1" "foo")))
(assert-equal '("foo" t)
	      (rdebug-get-script-name
	       '("ruby" "rdebug" "--annotate" "1" "foo")))
(assert-equal '("foo" nil)
	      (rdebug-get-script-name
	       '("/usr/bin/ruby19" "rdebug"
		 "--emacs-basic" "foo")))
(assert-equal '("foo" nil)
	      (rdebug-get-script-name '("rdbg.rb" "foo")))
(assert-equal '("rdbg.rb" nil)
	      (rdebug-get-script-name
	       '("rdebug" "rdbg.rb" "foo")))
(assert-equal '("foo" t)
	      (rdebug-get-script-name
	       '("rdebug" "-A" "1" "foo")))
(assert-equal '("foo" nil)
	      (rdebug-get-script-name
	       '("rdebug" "--include" "me" "-n" "foo")))
(assert-equal '("foo" nil)
	      (rdebug-get-script-name
	       '("rdebug" "--server" "-d" "--host"
		 "localhost" "foo" "-1")))

(end-tests)
