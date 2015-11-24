;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "./regexp-helper.el")
(load-file "../realgud/common/regexp.el")
(load-file "../realgud/debugger/trepan/init.el")

(declare-function realgud-cmdbuf-info-loc-regexp 'realgud-buffer-command)
(declare-function cmdbuf-loc-match               'realgud-regexp)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)
(declare-function realgud-cmdbuf-info-file-group 'realgud-regexp)
(declare-function realgud-cmdbuf-info-line-group 'realgud-regexp)
(declare-function realgud-cmdbuf-info            'realgud-regexp)
(declare-function make-realgud-cmdbuf-info       'realgud-regexp)
(declare-function __FILE__                       'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar loc-pat)
  (defvar test-dbgr)
  (defvar test-text)
)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger trepan. Others may follow.
;; FIXME: encapsulate this.
(setq dbg-name "trepan")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))

(setq test-text "-- (/usr/local/bin/irb:9 @2)")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))
(assert-equal "/usr/local/bin/irb"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text))

(assert-equal "9"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number")

(setq test-text "-> (<internal:lib/rubygems/custom_require>:28 remapped /usr/lib/ruby/gems/1.9.1/gems/data/custom_require.rb:28 @2)")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "remapped location")

(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))
(assert-equal "/usr/lib/ruby/gems/1.9.1/gems/data/custom_require.rb"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text) "extract remapped file name")

(assert-equal "28"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract remapped line number")

(setq test-text "C> (/tmp/c-func.rb:2)")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location for C fn")

(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))
(assert-equal "/tmp/c-func.rb"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text) "extract file name for C fn")

(assert-equal "2"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number for C fn")

(end-tests)
