;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Python "pydbgr" Debugger tracking a comint
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
(require-relative-list '("core" "init") "dbgr-pydbgr-")

(dbgr-track-mode-vars "pydbgr")

(declare-function dbgr-track-mode(bool))

(dbgr-python-populate-command-keys pydbgr-track-mode-map)

(defun pydbgr-track-mode-hook()
  (if pydbgr-track-mode
      (progn
	(use-local-map pydbgr-track-mode-map)
	(message "using pydbgr mode map")
	)
    (message "pydbgr track-mode-hook disable called")
    )
)

(define-minor-mode pydbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " pydbgr"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pydbgr
  :keymap pydbgr-track-mode-map
  (dbgr-track-set-debugger "pydbgr")
  (if pydbgr-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (pydbgr-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-pydbgr-")
