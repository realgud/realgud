;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/jdb/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)
(declare-function prompt-match          'regexp-helper)

(eval-when-compile
  (defvar dbg-name)   (defvar realgud-pat-hash)   (defvar loc-pat)
  (defvar test-dbgr)  (defvar test-text)         (defvar prompt-pat)
  (defvar realgud:jdb-pat-hash)
)

(test-simple-start)

(note "jdb prompt matching")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:jdb-pat-hash))
(prompt-match "main[1] "  "1" "most common main prompt")
(prompt-match "main[2] "  "2" "main prompt up a level")
(prompt-match "> " nil "no loc prompt")


; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "jdb")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(let ((text "Breakpoint hit: \"thread=main\", TestMe.main(), line=7 bci=0"))

  (note "traceback location matching")

  (assert-t (numberp (cmdbuf-loc-match text test-dbgr)) "breakpoint location")
  (assert-equal "7"
	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
			    text) "extract line number from breakpoint"))

(let ((text "Step completed: \"thread=main\", TestMe.main(), line=71 bci=0"))
  (assert-t (numberp (cmdbuf-loc-match text test-dbgr)) "breakpoint location")
  (assert-equal "71"
		(match-string (realgud-cmdbuf-info-line-group test-dbgr)
			      text) "extract line number from step"))

;; (note "debugger-backtrace")
;; (setq realgud-bt-pat  (gethash "debugger-backtrace"
;; 			    realgud:jdb-pat-hash))
;; (setq s1
;;       "  [1] java.lang.Class.getDeclaredMethods0 (native method)
;;   [2] java.lang.Class.privateGetDeclaredMethods (Class.java:2,570)
;;   [3] java.lang.Class.getMethod0 (Class.java:2,813)
;;   [4] java.lang.Class.getMethod (Class.java:1,663)
;;   [5] sun.launcher.LauncherHelper.getMainMethod (LauncherHelper.java:494)
;;   [6] sun.launcher.LauncherHelper.checkAndLoadMain (LauncherHelper.java:486)
;; ")
;; (setq realgud-bt-re (realgud-loc-pat-regexp realgud-bt-pat))
;; (setq file-group (realgud-loc-pat-file-group realgud-bt-pat))
;; (setq line-group (realgud-loc-pat-line-group realgud-bt-pat))
;; (assert-equal 0 (string-match realgud-bt-re s1))
;; (assert-equal "570"
;; 	      (substring s1
;; 			 (match-beginning line-group)
;; 			 (match-end line-group)))

(end-tests)
