;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/zshdb/init.el")
(load-file "./regexp-helper.el")

(declare-function loc-match	                 'realgud-helper)
(declare-function prompt-match                   'regexp-helper)
(declare-function realgud-loc-pat-num            'realgud-regexp)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)
(declare-function __FILE__                       'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar file-group)
  (defvar frame-re)
  (defvar line-group)
  (defvar num-group)
  (defvar test-pos)
  (defvar prompt-pat)
  (defvar realgud:zshdb-pat-hash)
  (defvar realgud-pat-bt)
  (defvar test-s1)
  (defvar test-text)
  (defvar brkpt-del)
  (defvar bp-del-pat)
)

(set (make-local-variable 'bp-del-pat)
      (gethash "brkpt-del" realgud:zshdb-pat-hash))

(setq prompt-pat (gethash "prompt"             realgud:zshdb-pat-hash))
(setq frame-pat  (gethash "debugger-backtrace" realgud:zshdb-pat-hash))

(set (make-local-variable 'bp-del-pat)
      (gethash "brkpt-del" realgud:zshdb-pat-hash))

(set (make-local-variable 'bp-enable-pat)
      (gethash "brkpt-enable" realgud:zshdb-pat-hash))

(set (make-local-variable 'bp-disable-pat)
      (gethash "brkpt-disable" realgud:zshdb-pat-hash))

(note "zshdb prompt matching")
(prompt-match "zshdb<10> "  "10")
(prompt-match	"zshdb<(5)> " "5" "subshell prompt %s")
(prompt-match	"zshdb<<1>> " "1" "nested debug prompt %s")

(note "zshdb frame matching")

(note "debugger-backtrace")
(setq test-s1
      "->0 in file `/etc/apparmor/functions' at line 24
##1 /etc/apparmor/functions called from file `/etc/init.d/apparmor' at line 35
##2 /etc/init.d/apparmor called from file `/usr/local/bin/zshdb' at line 129
")
(setq frame-re (realgud-loc-pat-regexp frame-pat))
(setq num-group (realgud-loc-pat-num frame-pat))
(setq file-group (realgud-loc-pat-file-group frame-pat))
(setq line-group (realgud-loc-pat-line-group frame-pat))
(assert-equal 0 (string-match frame-re test-s1))
(assert-equal "0" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/etc/apparmor/functions"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "24"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))

(assert-equal 49 (string-match frame-re test-s1 test-pos))
(assert-equal "1" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/etc/init.d/apparmor"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "35"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))
(assert-equal 128 (string-match frame-re test-s1 test-pos))
(assert-equal "2" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/usr/local/bin/zshdb"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "129"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))

(note "breakpoint delete matching")
(setq test-text "Deleted breakpoint 1\n")
(assert-t (numberp (loc-match test-text bp-del-pat)) "breakpoint delete matching")

(note "breakpoint enable matching")
(setq test-text "Breakpoint entry 4 enabled.\n")
(assert-t (numberp (loc-match test-text bp-enable-pat)) "breakpoint enable matching")


(note "breakpoint disable matching")
(setq test-text "Breakpoint entry 2 disabled.\n")
(assert-t (numberp (loc-match test-text bp-disable-pat)) "breakpoint disable matching")

(end-tests)
