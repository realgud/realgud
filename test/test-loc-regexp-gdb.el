;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "./regexp-helper.el")
(load-file "../realgud/common/regexp.el")
(load-file "../realgud/debugger/gdb/init.el")

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
  (defvar helper-bps)
  (defvar helper-info-brkpt)
  (defvar loc-pat)
  (defvar prompt-pat)
  (defvar realgud:gdb-pat-hash)
  (defvar test-dbgr)
  (defvar test-s1)
  (defvar test-text)
)

(note "prompt matching")

(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:gdb-pat-hash))

(prompt-match "(gdb) ")

(setup-regexp-vars realgud:gdb-pat-hash)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger trepan. Others may follow.
;; FIXME: encapsulate this.
(setq dbg-name "gdb")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))


(setq test-text "/tmp/Porosity.cpp:229:8538:beg:0x555555588d48")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))
(assert-equal "/tmp/Porosity.cpp"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text))

(assert-equal "229"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number")

(note "breakpoint location matching")

(setq test-s1
      "Breakpoint 4 at 0x34cc3: file Porosity.cpp, line 226.\n")

(assert-t (numberp (loc-match test-s1 helper-bps))
	  "basic breakpoint location")
(assert-equal "4"
	      (match-string (realgud-loc-pat-num (car helper-bps))
			    test-s1)   "extract breakpoint number")
(assert-equal "Porosity.cpp"
	      (match-string (realgud-loc-pat-file-group (car helper-bps))
			    test-s1)   "extract breakpoint file name")
(assert-equal "226"
	      (match-string (realgud-loc-pat-line-group (car helper-bps))
			    test-s1)
	      "extract breakpoint line number")


(setq test-s1
      "Breakpoint 10, main (argc=3, argv=0x7fffffffe738) at Porosity2.cpp:22.\n")

(assert-t (numberp (loc-match test-s1 helper-bps))
	  "basic breakpoint location")

(assert-equal "10"
	      (match-string 1 test-s1)   "extract breakpoint number")

(assert-equal "Porosity2.cpp"
	      (match-string 2 test-s1)   "extract breakpoint file name")
(assert-equal "22"
	      (match-string 3  test-s1)
	      "extract breakpoint line number")

(setq test-s1
      "1       breakpoint     keep y   0x0000000000401471 in vcdnav_get_entries at ctest.c:67")

(assert-t (numberp (loc-match test-s1 helper-info-brkpt))
	  "basic breakpoint location")
(assert-equal "1"
	      (match-string 1 test-s1)   "extract breakpoint number")

(assert-equal "ctest.c"
	      (match-string 5 test-s1)   "extract breakpoint file name")
(assert-equal "67"
	      (match-string 6  test-s1)
	      "extract breakpoint line number")


(end-tests)
