;; -*- lexical-binding:t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'load-relative)
(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/trepan/init.el")
(load-file "./regexp-helper.el")

(declare-function cmdbuf-loc-match               'realgud-regexp)
(declare-function make-realgud-cmdbuf-info       'realgud-regexp)
(declare-function realgud-cmdbuf-info            'realgud-regexp)
(declare-function realgud-cmdbuf-info-file-group 'realgud-regexp)
(declare-function realgud-cmdbuf-info-line-group 'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function __FILE__                       'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar loc-pat)
  (defvar test-dbgr)
)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group  loc-pat)
		  :line-group (realgud-loc-pat-line-group  loc-pat)))


(let ((text ".. (./dbgr.rb:73)")
	      (text2 "C> ((eval):1 via /tmp/eval2.rb:2)")
	      (text3 "-- (<internal:prelude>:28 remapped prelude.rb:28)")
	      (text4 "-- (/src/external-vcs/dbgrr/processor/command/info_subcmd/registers_subcmd/dfp.rb:2)\nrequire_relative %w(.. .. base subsubcmd)\n")
	      )

  (assert-t (numberp (cmdbuf-loc-match text test-dbgr)) "basic location")
  (assert-equal "./dbgr.rb"
		(match-string (realgud-cmdbuf-info-file-group test-dbgr)
			      text)   "extract file name")
  (assert-equal "73"
		(match-string (realgud-cmdbuf-info-line-group test-dbgr)
			      text)   "extract line number")
  (assert-t (numberp (cmdbuf-loc-match text4 test-dbgr)) "more complex location")


  ;; Now try 'via'
  (assert-t (numberp (cmdbuf-loc-match text2 test-dbgr)) "basic 'via' location")
  (assert-equal "/tmp/eval2.rb"
		(match-string (realgud-cmdbuf-info-file-group test-dbgr)
			      text2)
		  "extract via file name")
  (assert-equal "2" (match-string (realgud-cmdbuf-info-line-group test-dbgr)
				  text2)
		"extract via line number")

  ;; Now try remap
  (assert-t (numberp (cmdbuf-loc-match text3 test-dbgr)) "basic 'via' location")

  ;;
  (setq text "--> #0 METHOD Object#square(x) in file ./trepan.rb at line 73")
  (assert-nil (numberp (cmdbuf-loc-match text test-dbgr)) "unmatched location")

  )

(end-tests)
