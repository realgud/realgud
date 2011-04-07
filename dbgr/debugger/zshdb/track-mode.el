;;; "zshdb" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-zshdb-")

(dbgr-track-mode-vars "zshdb")
(dbgr-posix-shell-populate-command-keys zshdb-track-mode-map)

(declare-function dbgr-track-mode(bool))


(defun zshdb-track-mode-hook()
  (if zshdb-track-mode
      (progn
	(use-local-map zshdb-track-mode-map)
	(message "using zshdb mode map")
	)
    (message "zshdb track-mode-hook disable called"))
)

(define-minor-mode zshdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " zshdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'zshdb
  :keymap zshdb-track-mode-map

  (dbgr-track-set-debugger "zshdb")
  (if zshdb-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (zshdb-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-zshdb-")
