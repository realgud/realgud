(require 'load-relative)
(require-relative-list
 '("helper" "buffer/locals") "realgud-")

(defvar realgud-locals-mode-map
  (let ((map  (realgud-populate-debugger-menu (make-sparse-keymap))))
    (suppress-keymap map)
    (realgud-populate-common-keys map)
    (define-key map "q"       'realgud:cmd-quit)
    (define-key map "L"       'realgud:window-locals)
    map)
  )

(defun realgud-locals-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'realgud-locals-mode)
  (setq mode-name "Realgud Locals")
  (use-local-map realgud-locals-mode-map)
  )

(provide-me "realgud-")
