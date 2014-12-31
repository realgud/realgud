(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/gdb/core.el")
(load-file "./regexp-helper.el")

(eval-when-compile (defvar realgud:gdb-minibuffer-history))

(declare-function realgud:gdb-suggest-invocation 'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

(setq realgud:gdb-minibuffer-history nil)

(note "realgud:gdb-suggest-invocation")
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
    (assert-equal "gdb bar.sh" (realgud:gdb-suggest-invocation))
    (setq realgud:gdb-minibuffer-history '("gdb testing"))
    (setq default-directory
	(concat my-directory  "gdb/test2"))
    (assert-equal "gdb testing" (realgud:gdb-suggest-invocation)
		  "After setting minibuffer history - takes precidence")
    )
  (setq default-directory my-directory)
)

(end-tests)

;; You might have to run the below if you run this interactively.
(setq default-directory (file-name-directory (__FILE__)))
