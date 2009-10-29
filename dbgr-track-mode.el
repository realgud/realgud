;;  `dbgr-track-mode' tracks shell output 

(defun dbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'dbgr-track-mode))))
    (if file-name
        (file-name-directory file-name)
      nil)))

(eval-when-compile (require 'cl))
(setq load-path (cons nil (cons (dbgr-directory) load-path)))
(load "dbgr-track")
(load "dbgr-loc")
(load "dbgr-lochist")
(load "dbgr-file")
(load "dbgr-procbuf-var")
(load "dbgr-window")
(load "dbgr-regexp")
(require 'dbgr-regexp)
(setq load-path (cddr load-path))

(defvar dbgr-track-mode nil
  "Non-nil if using dbgr-track mode as a minor mode of some other mode.
Use the command `dbgr-track-minor-mode' to toggle or set this variable.")

(defvar dbgr-track-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right]	'dbgr-track-hist-newest)
    (define-key map [M-down]	'dbgr-track-hist-newer)
    (define-key map [M-up]	'dbgr-track-hist-older)
    (define-key map [M-S-down]	'dbgr-track-hist-newest)
    (define-key map [M-S-up]	'dbgr-track-hist-oldest)
    map)
  "Keymap used in `dbgr-track-minor-mode'.")

(define-minor-mode dbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  :lighter (:eval (progn (concat " " (dbgr-info-name dbgr-info))))
  ;; The minor mode bindings.
  :global nil
  :group 'dbgr
  :keymap dbgr-track-mode-map
  (if dbgr-track-mode
      (progn
	(add-hook 'comint-output-filter-functions 
		  'dbgr-track-comint-output-filter-hook)
	(add-hook 'eshell-output-filter-functions 
		  'dbgr-track-eshell-output-filter-hook)
  
	;; FIXME: dbgr-info is somehow set initiall so this code isn't 
	;; kicking in.
	(unless (boundp 'dbgr-info)
	  (call-interactively 'dbgr-track-set-debugger))
	(run-mode-hooks 'dbgr-track-mode-hook))
    (progn
      (remove-hook 'comint-output-filter-functions 
		   'dbgr-track-comint-output-filter-hook)
      (remove-hook 'eshell-output-filter-functions 
		    'dbgr-track-eshell-output-filter-hook))))

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'dbgr-track-mode)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-track.el ends here
