;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/common/shortkey.el")
(load-file "../realgud/common/buffer/command.el")
(declare-function realgud-cmdbuf-init            'realgud-buffer-command)
(declare-function realgud-shortkey-mode-setup    'realgud-shortkey)
(declare-function realgud-get-short-key-mode-map 'realgud-shortkey)

(eval-when-compile
  (defvar temp-cmdbuf)
  (defvar debugger-name)
  (defvar realgud-pat-hash)
  (defvar test-keymap)
  (defvar nodejs-short-key-mode-map)
)

(declare-function __FILE__           'load-relative)

(test-simple-start)

(note "realgud-shortkey")
(assert-raises error (realgud-shortkey-mode-setup))
(assert-nil (realgud-get-short-key-mode-map (current-buffer)))

(note "realgud-get-short-key-mode-map")

(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(setq debugger-name "nodejs")
(load-file "../realgud/debugger/nodejs/nodejs.el")
(realgud-cmdbuf-init temp-cmdbuf debugger-name
		     (gethash debugger-name realgud-pat-hash))
(setq test-keymap (realgud-get-short-key-mode-map temp-cmdbuf))
(assert-t (keymapp test-keymap)
	  "realgud-get-short-key-mode-map returns keymap")
(assert-equal test-keymap nodejs-short-key-mode-map
	  "realgud-get-short-key-mode-map returns nodejs-short-key-mode-map")
(end-tests)
