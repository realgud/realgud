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
(require-relative-list '("core" "cmds" "init") "dbgr-remake-")

(dbgr-track-mode-vars "remake")
(set-keymap-parent remake-track-mode-map dbgr-track-mode-map)

(declare-function dbgr-track-mode(bool))

;; (define-key remake-track-mode-map 
;;   (kbd "C-c !!") 'remake-goto-dollarbang-backtrace-line)
;; (define-key remake-track-mode-map 
;;   (kbd "C-c !b") 'remake-goto-backtrace-line)
;; (define-key remake-track-mode-map 
;;   (kbd "C-c !c") 'remake-goto-control-frame-line)
;; (define-key remake-track-mode-map 
;;   (kbd "C-c !c") 'remake-goto-control-frame-line)

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
