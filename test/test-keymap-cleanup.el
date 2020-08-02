;; Regression test for https://github.com/realgud/realgud/issues/275

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "loc-changes.elc")) buffer-file-name)

(require 'test-simple)

(test-simple-start)

(note "realgud-keymap-cleanup")

(setq temp-procbuf (generate-new-buffer "*procbuf-test*"))
(setq temp-srcbuf (generate-new-buffer "*srcbuf-test*"))
(with-current-buffer temp-srcbuf
  (python-mode) )
(setq debugger-name "pdb")
(load-file "../realgud/debugger/pdb/pdb.el")

(realgud-cmdbuf-init temp-procbuf debugger-name
		     (gethash debugger-name realgud-pat-hash))
(realgud-srcbuf-init-or-update temp-srcbuf temp-procbuf)

(note "Realgud keys present in srcbuf")
(with-current-buffer temp-srcbuf
  (assert-t (equal 'realgud:cmd-step (key-binding (kbd "<f11>")))) )

(realgud:terminate-srcbuf temp-srcbuf)

(note "Realgud keys not present in srcbuf after quit")
(with-current-buffer temp-srcbuf
  (python-mode)
  (assert-nil (equal 'realgud:cmd-step (key-binding (kbd "<f11>")))) )

(end-tests)
