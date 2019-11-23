;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'comint)
(require 'eshell)
(require 'shell)

(load-file "../realgud/common/utils.el")

(declare-function realgud:flatten            'realgud-utils)
(declare-function realgud:strip              'realgud-regexp)
(declare-function __FILE__                   'load-relative)
(declare-function realgud:canonic-major-mode 'realgud-utils)
(declare-function realgud:join-string        'realgud-utils)

(test-simple-start)

(eval-when-compile
  (defvar test-realgud:features)
)

(note "realgud:strip")
(assert-equal "abc" (realgud:strip "abc"))
(assert-equal "def" (realgud:strip "\n  def\t  "))

(note "realgud:join-string")
(assert-equal "a b c" (realgud:join-string '("a" "b" "c") " "))

(note "realgud:flatten")
(assert-equal '(abc) (realgud:flatten '(abc)))
(assert-equal '(abc def h i j) (realgud:flatten '(abc (def (h) i) j)))

(note "realgud:canonic-major-mode")

(assert-raises error (realgud:canonic-major-mode)
	       "Not in eshell, comint or shell-mode")

(with-temp-buffer
  (comint-mode)
  (assert-equal 'comint (realgud:canonic-major-mode)))

(with-temp-buffer
  (eshell-mode)
  (assert-equal 'eshell (realgud:canonic-major-mode)))

;; (with-temp-buffer
;;   (start-process "bogus" (current-buffer) "sleep" "1")
;;   (shell-mode)
;;   (assert-equal 'comint (realgud:canonic-major-mode))
;;   )

(note "realgud:remove-ansi-schmutz-in-string")
(assert-equal "(gdb) " (realgud:remove-ansi-schmutz-in-string "[?2004h[?2004l[?2004h(gdb) "))
(assert-equal "color" (realgud:remove-ansi-schmutz-in-string "[30;46mcolor[0m"))


(end-tests)
