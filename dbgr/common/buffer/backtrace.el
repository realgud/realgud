;;; Backtrace buffer
;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../send" "../track" "../key") "dbgr-")
(require-relative-list
 '("command") "dbgr-buffer-")

;: FIXME: not picked up from track. Why?
(defvar dbgr-track-divert-string nil)

(defvar dbgr-bt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [double-mouse-1] 'dbgr-goto-frame-mouse)
    (define-key map [mouse-2] 'dbgr-goto-frame-mouse)
    (define-key map [mouse-3] 'dbgr-goto-frame-mouse)
    (define-key map [(control m)] 'dbgr-goto-frame)
    (define-key map "0" 'dbgr-goto-frame-n)
    (define-key map "1" 'dbgr-goto-frame-n)
    (define-key map "2" 'dbgr-goto-frame-n)
    (define-key map "3" 'dbgr-goto-frame-n)
    (define-key map "4" 'dbgr-goto-frame-n)
    (define-key map "5" 'dbgr-goto-frame-n)
    (define-key map "6" 'dbgr-goto-frame-n)
    (define-key map "7" 'dbgr-goto-frame-n)
    (define-key map "8" 'dbgr-goto-frame-n)
    (define-key map "9" 'dbgr-goto-frame-n)
    (dbgr-populate-common-keys map)

    ;; --------------------
    ;; The "Stack window" submenu.
    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger stack]
        (cons "Stack window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger stack goto]
      '(menu-item "Goto frame" dbgr-goto-frame))
    map)
  "Keymap to navigate dbgr stack frames.")

(defun dbgr-bactrace-init ()
  (interactive)
  (let ((buffer (current-buffer))
  	(cmdbuf (dbgr-get-cmdbuf))
  	(process)
  	)
    (with-current-buffer-safe cmdbuf
      (setq process (get-buffer-process (current-buffer)))
      (dbgr-cmdbuf-info-in-srcbuf?= dbgr-cmdbuf-info 
    				   (not (dbgr-cmdbuf? buffer)))
      (dbgr-cmdbuf-info-divert-output?= dbgr-cmdbuf-info 't)
      (setq dbgr-track-divert-string nil)
      (dbgr-command "backtrace" nil nil 't)
      (while (and (eq 'run (process-status process))
		    (null dbgr-track-divert-string))
	  (sleep-for 0.001)
	  )
      ;; (message "+++4 %s" dbgr-track-divert-string)
      (let ((bt-buffer (get-buffer-create
			(format "*%s backtrace*" (buffer-name))))
	    (divert-string dbgr-track-divert-string)
	    )
	(with-current-buffer bt-buffer
	  (setq buffer-read-only nil)
	  (delete-region (point-min) (point-max))
	  (if divert-string (insert divert-string))
	  )
	)
    )
  )
)

(provide-me "dbgr-buffer-")
