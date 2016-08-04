;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/gdb/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)   (defvar realgud-pat-hash)   (defvar realgud-bt-pat)
  (defvar loc-pat)    (defvar prompt-pat)         (defvar test-s1)
  (defvar file-group) (defvar line-group)         (defvar test-pos)
  (defvar test-dbgr)  (defvar test-text)          (defvar realgud-bt-re)
  (defvar realgud:gdb-pat-hash)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "gdb")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group loc-pat)
		  :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq test-text "/home/rocky/c/ctest.c:80:2000:beg:0x8048748>")
(note "traceback location matching")

(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
(assert-equal "/home/rocky/c/ctest.c"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text) "extract file name")
(assert-equal "80"
	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
			    test-text) "extract line number")
(note "debugger-backtrace")
(setq realgud-bt-pat  (gethash "debugger-backtrace"
			    realgud:gdb-pat-hash))
(setq test-s1
      "#0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
#1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 \"/tmp/remake/remake\") at strdup.c:42
#2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
    at main.c:952
#46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0,
    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410
")
(setq realgud-bt-re (realgud-loc-pat-regexp realgud-bt-pat))
(setq file-group (realgud-loc-pat-file-group realgud-bt-pat))
(setq line-group (realgud-loc-pat-line-group realgud-bt-pat))
(assert-equal 0 (string-match realgud-bt-re test-s1))
(assert-equal "main.c"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "935"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 65 test-pos)
(assert-equal 65 (string-match realgud-bt-re test-s1 test-pos))
(assert-equal "strdup.c"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "42"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 149 test-pos)
(assert-equal 149 (string-match realgud-bt-re test-s1 test-pos))
(assert-equal "main.c"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "952"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 233 test-pos)
(assert-equal 233 (string-match realgud-bt-re test-s1 test-pos))
(assert-equal "vm_insnhelper.c"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "410"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:gdb-pat-hash))
(prompt-match "(gdb) ")

(end-tests)
