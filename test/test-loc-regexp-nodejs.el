;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(load-file "./regexp-helper.el")
(load-file "../realgud/common/regexp.el")
(load-file "../realgud/debugger/nodejs/init.el")

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
(declare-function realgud-loc-pat-char-offset-group  'realgud:nodejs-init)

(test-simple-start)

(eval-when-compile
  (defvar file-group)
  (defvar frame-re)
  (defvar line-group)
  (defvar num-group)
  (defvar col-group)
  (defvar test-pos)
  (defvar bt-re)
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar loc-pat)
  (defvar test-dbgr)
  (defvar test-s1)
  (defvar realgud-pat-bt)
  (defvar realgud:nodejs-pat-hash)
)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "nodejs")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))


(note "debugger-backtrace")
(setq realgud-pat-bt  (gethash "debugger-backtrace"
			     realgud:nodejs-pat-hash))
(setq test-s1
      "#0 module.js:380:17
#1 Module._compile module2.js:456:26
#2 Module._extensions..js module.js:474:10
#3 Module.load module.js:356:32
")

(setq bt-re (realgud-loc-pat-regexp realgud-pat-bt))
(setq num-group (realgud-loc-pat-num realgud-pat-bt))
(setq file-group (realgud-loc-pat-file-group realgud-pat-bt))
(setq line-group (realgud-loc-pat-line-group realgud-pat-bt))
(setq col-group (realgud-loc-pat-char-offset-group realgud-pat-bt))
(assert-equal 0 (string-match bt-re test-s1))
(assert-equal "0" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "module.js"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "380"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(assert-equal "17" (substring test-s1
			     (match-beginning col-group)
			     (match-end col-group)))

(setq test-pos (match-end 0))
(assert-equal 19 (string-match bt-re test-s1 test-pos))

(setq test-s1
      "#1 Module._compile module2.js:456:26
#2 Module._extensions..js module.js:474:10
#3 Module.load module.js:356:32
")
(assert-equal 0 (string-match bt-re test-s1))

(assert-equal "1" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "module2.js"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "456"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(assert-equal "26" (substring test-s1
			      (match-beginning col-group)
			      (match-end col-group)))
(setq test-pos (match-end 0))
(assert-equal 36 test-pos)

(end-tests)
