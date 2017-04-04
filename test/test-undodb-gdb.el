;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/undodb-gdb/core.el")
(load-file "./regexp-helper.el")

(eval-when-compile
  (defvar realgud:undodb-gdb-minibuffer-history)
  (defvar test:realgud-undodb-gdb-executable-save)
  (defvar test:realgud-minibuffer-history-save)
  )

(declare-function realgud:undodb-gdb-suggest-invocation 'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:realgud-undodb-gdb-executable-save (symbol-function 'realgud:undodb-gdb-executable))
(setq test:realgud-minibuffer-history-save realgud:undodb-gdb-minibuffer-history)

(defun realgud:undodb-gdb-executable (filename)
  "Mock function for testing"
  (cond ((equal filename "bar.sh") 7)
	((equal filename "foo") 8)
	((equal filename "baz") 8)
	(t 3)))

(defun undodb-gdb-test()
  (note "realgud:undodb-gdb-suggest-invocation")
  (setq realgud:undodb-gdb-minibuffer-history nil)
  (let ((my-directory (file-name-directory (__FILE__))))
    (save-excursion
      (note "Test preference to buffer editing")
      (setq default-directory
	    (concat my-directory "gdb"))
      (find-file-literally "foo.c")
      (assert-equal "undodb-gdb foo" (realgud:undodb-gdb-suggest-invocation)
		    "Should find file sans extension - foo")
      (find-file-literally "baz.c")
      (assert-equal "undodb-gdb baz" (realgud:undodb-gdb-suggest-invocation)
		    "Should find file sans extension - baz")
      )
    (save-excursion
      (note "Pick up non-sans executable")
      (setq default-directory
	    (concat my-directory  "undodb-gdb/test2"))
      ;; (assert-equal "undodb-gdb bar.sh" (realgud:undodb-gdb-suggest-invocation))
      (setq realgud:undodb-gdb-minibuffer-history '("undodb-gdb testing"))
      (setq default-directory
	    (concat my-directory  "undodb-gdb/test2"))
      (assert-equal "undodb-gdb testing" (realgud:undodb-gdb-suggest-invocation)
		    "After setting minibuffer history - takes precidence")
      )
    (setq default-directory my-directory)
    )
  )
(undodb-gdb-test)
(end-tests)

;; Restore the old values.
;; You might have to run the below if you run this interactively.
(fset 'realgud:undodb-gdb-executable test:realgud-undodb-gdb-executable-save)
(setq realgud:undodb-gdb-minibuffer-history test:realgud-minibuffer-history-save)
(setq default-directory (file-name-directory (__FILE__)))
