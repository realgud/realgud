;; -*- lexical-binding:t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(eval-when-compile (defvar trepan-core))

(declare-function __FILE__ 'load-relative)
(setq trepan-core "../realgud/debugger/trepan/core.el")
(load-file "../realgud/common/core.el")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg,     'realgud-core)
(declare-function realgud:trepan-parse-cmd-args  'realgud:trepan)

;; We use a specific language to test core. Here we use trepan.
(load-file "../realgud/debugger/trepan/core.el")

(test-simple-start)

;; FIXME: Add a test of relgud-exec-shell where
;; we have two invocation of different files that canonicalize
;; to the same buffer. Make sure the buffers are distinct.
;; For example: bashdb /etc/profile should not match
;; bashdb /tmp/profile

(note "realgud:expand-file-name-if-exists")

(assert-equal (realgud:expand-file-name-if-exists "file-not-here")
	      "file-not-here" "no expansion when expanded file doesn't exist")

(assert-equal (realgud:expand-file-name-if-exists ".")
	      (expand-file-name "."))

(note "realgud-parse-...")

(let ((opt-two-args '("0" "C" "e" "E" "F" "i")))
  (assert-equal '(("-0" "a") nil)
		(realgud-parse-command-arg '("-0" "a") '() opt-two-args)
		  "Two args found, none remain afterwards though.")

  (assert-equal
   '(("-5") ("a" "-0"))
   (realgud-parse-command-arg '("-5" "a" "-0") '()
				    opt-two-args)
   "One arg not found.")

  (assert-equal
   '((nil) nil)
   (realgud-parse-command-arg '() '() opt-two-args)
   "Degenerate case - no args"
   )

  (assert-equal
   '(("--port" "123") ("bar"))
   (realgud-parse-command-arg
    '("--port" "123" "bar") '("-port") '())
   "two mandatory args"
   )

  (assert-equal
   '(("/usr/bin/ruby1.9" "-W") ("trepan") ("foo") nil)
   (realgud:trepan-parse-cmd-args
    '("/usr/bin/ruby1.9" "-W" "trepan" "foo"))
     "Separate Ruby with its arg from debugger and its arg.")

  (assert-equal
   '(("ruby1.9" "-T3") ("trepan" "--port" "123") ("bar") nil)
   (realgud:trepan-parse-cmd-args
    '("ruby1.9" "-T3" "trepan" "--port" "123" "bar"))
   "Ruby with two args and trepan with two args")

  (assert-equal
   '(nil ("trepan" "--port" "1" "--annotate=3")
	 ("foo" "a") t)
   (realgud:trepan-parse-cmd-args
    '("trepan" "--port" "1" "--annotate=3" "foo" "a"))
  "trepan with annotate args")

  (assert-equal
   '(nil ("trepan" "--port" "123")
	 ("foo" "--emacs" "a") nil)
   (realgud:trepan-parse-cmd-args
    '("trepan" "--port" "123" "foo" "--emacs" "a"))
   "trepan with --emacs in the wrong place")

  (assert-equal
   '(("ruby" "-I/usr/lib/ruby")
     ("trepan" "-h" "foo" "--emacs")
     ("baz") t)
   (realgud:trepan-parse-cmd-args
    '("ruby" "-I/usr/lib/ruby" "trepan" "-h" "foo"
      "--emacs" "baz"))
     "trepan with emacs")
  )

(end-tests)
