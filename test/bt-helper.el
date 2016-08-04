(require 'test-simple)
(require 'font-lock)

(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/common/buffer/backtrace.el")
(load-file "../realgud/common/backtrace-mode.el")

(declare-function realgud-backtrace-mode 'realgud-backtrace-mode)
(declare-function realgud-cmdbuf-init 'realgud-buffer-command)

(eval-when-compile
  (defvar temp-cmdbuf)
  (defvar temp-bt)
  (defvar realgud-pat-hash)
)

(defun setup-bt-vars(debugger-name)
  "Sets up globals temp-cmdbuf and temp-bt with command buffer
for DEBUGGER-NAME"
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (setq temp-bt (generate-new-buffer "*bt-test*"))
  (with-current-buffer temp-cmdbuf
    (switch-to-buffer temp-cmdbuf)
    (realgud-cmdbuf-init temp-cmdbuf debugger-name
		      (gethash debugger-name realgud-pat-hash))

    (switch-to-buffer nil)
  ))


(defun setup-bt(debugger-name string)
  "Sets up globals temp-cmdbuf and temp-bt with command buffer
for DEBUGGER-NAME and initializes it to STRING"
  (setup-bt-vars debugger-name)
  (with-current-buffer temp-bt
    (realgud-backtrace-mode temp-cmdbuf)
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (insert string)
    (font-lock-fontify-buffer)
    ;; Newer emacs's use:
    (goto-char (point-min))
    )
  temp-bt
)

(provide 'realgud-bt-helper)
