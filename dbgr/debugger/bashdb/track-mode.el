;;; Ruby "bashdb" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "cmds" "init") "dbgr-bashdb-")

(dbgr-track-mode-vars "bashdb")
(set-keymap-parent bashdb-track-mode-map dbgr-track-mode-map)

(declare-function dbgr-track-mode(bool))

(define-key bashdb-track-mode-map 
  (kbd "C-c !!") 'bashdb-goto-dollarbang-backtrace-line)
(define-key bashdb-track-mode-map 
  (kbd "C-c !b") 'bashdb-goto-backtrace-line)
(define-key bashdb-track-mode-map 
  (kbd "C-c !c") 'bashdb-goto-control-frame-line)
(define-key bashdb-track-mode-map 
  (kbd "C-c !c") 'bashdb-goto-control-frame-line)

(defun bashdb-track-mode-hook()
  (if bashdb-track-mode
      (progn
	(use-local-map bashdb-track-mode-map)
	(message "using bashdb mode map")
	)
    (message "bashdb track-mode-hook disable called"))
)

(define-minor-mode bashdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " bashdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'bashdb
  :keymap bashdb-track-mode-map

  (dbgr-track-set-debugger "bashdb")
  (if bashdb-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (bashdb-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-bashdb-")
