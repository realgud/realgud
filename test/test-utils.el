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

(test-simple-start)

(eval-when-compile
  (defvar test-realgud:features)
)

(note "realgud:strip")
(assert-equal "abc" (realgud:strip "abc"))
(assert-equal "def" (realgud:strip "\n  def\t  "))

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

(end-tests)
