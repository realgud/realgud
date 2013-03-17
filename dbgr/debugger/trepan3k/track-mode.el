;;; Copyright (C) 2010, 2012-2013 Rocky Bernstein <rocky@gnu.org>
;;; Python "trepan3k" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-trepan3k-")

(dbgr-track-mode-vars "trepan3k")

(declare-function dbgr-track-mode(bool))

(dbgr-python-populate-command-keys trepan3k-track-mode-map)

(defun trepan3k-track-mode-hook()
  (if trepan3k-track-mode
      (progn
	(use-local-map trepan3k-track-mode-map)
	(message "using trepan3k mode map")
	)
    (message "trepan3k track-mode-hook disable called")
    )
)

(define-minor-mode trepan3k-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepan3k"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan3k
  :keymap trepan3k-track-mode-map
  (dbgr-track-set-debugger "trepan3k")
  (if trepan3k-track-mode
      (progn 
	(setq dbgr-track-mode 't)
	(dbgr-track-mode-setup 't)
	(run-mode-hooks (intern (trepan3k-track-mode-hook))))
    (progn 
      (setq dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-trepan3k-")
