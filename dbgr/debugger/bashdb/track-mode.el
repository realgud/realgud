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
(require-relative-list '("core" "init") "dbgr-bashdb-")

(dbgr-track-mode-vars "bashdb")
(dbgr-posix-shell-populate-command-keys bashdb-track-mode-map)

(declare-function dbgr-track-mode(bool))

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
