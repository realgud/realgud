;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/gdb/core.el")
(load-file "./regexp-helper.el")

(eval-when-compile
  (defvar realgud:gdb-minibuffer-history)
  (defvar test:realgud-gdb-executable-save)
  (defvar test:realgud-minibuffer-history-save)
)

(declare-function realgud:gdb-suggest-invocation 'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:realgud-gdb-executable-save (symbol-function 'realgud:gdb-executable))
(setq test:realgud-minibuffer-history-save realgud:gdb-minibuffer-history)

(defun realgud:gdb-executable (filename)
  "Mock function for testing"
  (cond ((equal filename "bar.sh") 7)
	((equal filename "foo") 8)
	((equal filename "baz") 8)
	(t 3)))

(defun gdb-test()
  (note "realgud:gdb-suggest-invocation")
  (setq realgud:gdb-minibuffer-history nil)
  (let ((my-directory (file-name-directory (__FILE__))))
    (save-excursion
      (note "Test preference to buffer editing")
      (setq default-directory
	    (concat my-directory "gdb"))
      (find-file-literally "foo.c")
      (assert-equal "gdb foo" (realgud:gdb-suggest-invocation)
		    "Should find file sans extension - foo")
      (find-file-literally "baz.c")
      (assert-equal "gdb baz" (realgud:gdb-suggest-invocation)
		    "Should find file sans extension - baz")
      )
    (save-excursion
      (note "Pick up non-sans executable")
      (setq default-directory
	    (concat my-directory  "gdb/test2"))
      ;; (assert-equal "gdb bar.sh" (realgud:gdb-suggest-invocation))
      (setq realgud:gdb-minibuffer-history '("gdb testing"))
      (setq default-directory
	    (concat my-directory  "gdb/test2"))
      (assert-equal "gdb testing" (realgud:gdb-suggest-invocation)
		    "After setting minibuffer history - takes precidence")
      )
    (setq default-directory my-directory)
    )
  )
(gdb-test)
(end-tests)

;; Restore the old values.
;; You might have to run the below if you run this interactively.
(fset 'realgud:gdb-executable test:realgud-gdb-executable-save)
(setq realgud:gdb-minibuffer-history test:realgud-minibuffer-history-save)
(setq default-directory (file-name-directory (__FILE__)))
