(provide 'dbgr-window)
(require 'load-relative)
(require-relative-list '("dbgr-helper"))

(fn-p-to-fn?-alias 'one-window-p)
(declare-function one-window?(bool))

(defun dbgr-split-or-other-window()
  "Split the window if there is only one in the current
  frame. However if there is more than one window move to that"
  (interactive)
  ;; Anders code has more complicated logic for figuring out
  ;; which of serveral "other" windows is the one you want to switch
  ;; to.
  (if (one-window? 't) (split-window) (other-window 1)))

(defun dbgr-split-window()
  "Split the window if there is only one in the current
  frame."
  (interactive)
  ;; Anders code has more complicated logic for figuring out
  ;; which of serveral "other" windows is the one you want to switch
  ;; to.
  (if (one-window? 't) (split-window)))
