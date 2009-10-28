(defun dbgr-split-or-other-window()
  "Split the window if there is only one in the current
  frame. However if there is more than one window move to that"
  (interactive)
  ;; Anders code has more complicated logic for figuring out
  ;; which of serveral "other" windows is the one you want to switch
  ;; to.
  (if (one-window-p 't) (split-window) (other-window 1)))

