;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/bashdb/init.el")
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
  (defvar realgud:bashdb-pat-hash)
  (defvar realgud-pat-bt)
  (defvar test-s1)
  (defvar test-text)
  (defvar brkpt-del)
  (defvar bp-del-pat)
  (defvar bp-enable-pat)
  (defvar bp-disable-pat)
)

(set (make-local-variable 'bp-del-pat)
      (gethash "brkpt-del" realgud:bashdb-pat-hash))

(set (make-local-variable 'bp-enable-pat)
      (gethash "brkpt-enable" realgud:bashdb-pat-hash))

(set (make-local-variable 'bp-disable-pat)
      (gethash "brkpt-disable" realgud:bashdb-pat-hash))

(note "bashdb prompt matching")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:bashdb-pat-hash))
(prompt-match "bashdb<10> "  "10")
(prompt-match	"bashdb<(5)> " "5" "subshell prompt %s")
(prompt-match	"bashdb<<1>> " "1" "nested debug prompt %s")

(note "debugger-backtrace")
(setq realgud-pat-bt  (gethash "debugger-backtrace"
			     realgud:bashdb-pat-hash))
(setq test-s1
      "->0 in file `/etc/apparmor/functions' at line 24
##1 source(\"/etc/apparmor/functions\") called from file `/etc/init.d/apparmor' at line 35
##2 source(\"/etc/init.d/apparmor\") called from file `/usr/local/bin/bashdb' at line 140
##3 main() called from file `/bin/bashdb' at line 0
")
(setq frame-re (realgud-loc-pat-regexp realgud-pat-bt))
(setq num-group (realgud-loc-pat-num realgud-pat-bt))
(setq file-group (realgud-loc-pat-file-group realgud-pat-bt))
(setq line-group (realgud-loc-pat-line-group realgud-pat-bt))
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
(assert-equal 138 (string-match frame-re test-s1 test-pos))
(assert-equal "2" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/usr/local/bin/bashdb"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "140"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))
(assert-equal 226 (string-match frame-re test-s1 test-pos))
(assert-equal "3" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/bin/bashdb"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))

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
