;;; Ruby "remake" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-remake-")

(dbgr-track-mode-vars "remake")

(declare-function dbgr-track-mode(bool))

(define-key remake-track-mode-map 
  (kbd "C-c !!") 'dbgr-goto-lang-backtrace-line)
(define-key remake-track-mode-map 
  (kbd "C-c !b") 'dbgr-goto-debugger-backtrace-line)

(defun remake-track-mode-hook()
  (if remake-track-mode
      (progn
	(use-local-map remake-track-mode-map)
	(message "using remake mode map")
	)
    (message "remake track-mode-hook disable called"))
)

(define-minor-mode remake-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " remake"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'remake
  :keymap remake-track-mode-map

  (dbgr-track-set-debugger "remake")
  (if remake-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (remake-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-remake-")
