;;  `dbgr-track-mode' tracks shell output 

(eval-when-compile (require 'cl))

(require 'load-relative)
(provide 'dbgr-track-mode)
(require-relative-list
 '("dbgr-helper" "dbgr-track" "dbgr-loc" "dbgr-lochist" "dbgr-file" 
   "dbgr-cmdbuf" "dbgr-window" "dbgr-regexp"))

(defvar dbgr-track-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right]	'dbgr-track-hist-newest)
    (define-key map [M-down]	'dbgr-track-hist-newer)
    (define-key map [M-up]	'dbgr-track-hist-older)
    (define-key map [M-S-down]	'dbgr-track-hist-newest)
    (define-key map [M-S-up]	'dbgr-track-hist-oldest)
    map)
  "Keymap used in `dbgr-track-minor-mode'.")

;; FIXME figure out if I can put this in something like a header file.
(declare-function dbgr-track-set-debugger (debugger-name &optional hash))

(define-minor-mode dbgr-track-mode
  "Minor mode for tracking debugging inside a process shell."
  :init-value nil
  :global nil
  :group 'dbgr

  :lighter 
  (:eval (progn 
	   (concat " "
		   (if (dbgr-cmdbuf-info-set?)
		       (dbgr-sget 'cmdbuf-info 'name)
		     "dbgr??"))))

  :keymap dbgr-track-mode-map
  
  (dbgr-track-mode-body)
  )

(defun dbgr-track-mode-body ()
  (if dbgr-track-mode
      (progn
	(if (boundp 'comint-last-output-start)
	    (let* ((regexp-hash
		   (and dbgr-cmdbuf-info 
		    (dbgr-sget 'cmdbuf-info 'regexp-hash)))
		   (prompt-pat (and regexp-hash 
				    (gethash "prompt" regexp-hash))))
	      (if prompt-pat
		  (setq comint-prompt-regexp (dbgr-loc-pat-regexp prompt-pat))))
	  (progn
	    (set-marker comint-last-output-start (point))
	    (dbgr-cmdbuf-info-prior-prompt-regexp= 
	     dbgr-cmdbuf-info comint-prompt-regexp)
	    ))
	(add-hook 'comint-output-filter-functions 
		  'dbgr-track-comint-output-filter-hook)
	(add-hook 'eshell-output-filter-functions 
		  'dbgr-track-eshell-output-filter-hook)
  
	(unless (and (dbgr-cmdbuf-info-set?)
		     (dbgr-sget 'cmdbuf-info 'name))
	  (call-interactively 'dbgr-track-set-debugger))
	(run-mode-hooks 'dbgr-track-mode-hook))
    (progn
      (unless (boundp 'comint-last-output-start)
	(setq comint-prompt-regexp
	   (dbgr-sget 'cmdbuf-info 'prior-prompt-regexp))
	)
      (remove-hook 'comint-output-filter-functions 
		   'dbgr-track-comint-output-filter-hook)
      (remove-hook 'eshell-output-filter-functions 
		    'dbgr-track-eshell-output-filter-hook)))
)

;; -------------------------------------------------------------------
;; The end.
;;

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-track-mode.el ends here
