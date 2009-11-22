(require 'load-relative)
(require-relative-list '("dbgr-helper"))

(fn-p-to-fn?-alias 'one-window-p)
(declare-function one-window?(bool))

(defun dbgr-split-or-other-window( &optional buffer )
  "Split the window if there is only one in the current
frame. However if there is more than one window move to that.  In
either case, make that window display buffer"
  (interactive)
  ;; Anders code has more complicated logic for figuring out
  ;; which of serveral "other" windows is the one you want to switch
  ;; to.
  (if (one-window? 't) (split-window) (other-window 1))
  (if buffer
      (set-window-buffer (selected-window) buffer))
  )

(provide-me)
