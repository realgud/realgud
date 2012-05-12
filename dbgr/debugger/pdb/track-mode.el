;;; Copyright (C) 2010, 2012 Rocky Bernstein <rocky@gnu.org>
;;; Python "pdb" Debugger tracking a comint
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
(require-relative-list '("core" "init") "dbgr-pdb-")

(dbgr-track-mode-vars "pdb")

(declare-function dbgr-track-mode(bool))

(dbgr-python-populate-command-keys pdb-track-mode-map)

(defun pdb-track-mode-hook()
  (if pdb-track-mode
      (progn
	(use-local-map pdb-track-mode-map)
	(message "using pdb mode map")
	)
    (message "pdb track-mode-hook disable called")
    )
)

(define-minor-mode pdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " pdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pdb
  :keymap pdb-track-mode-map
  (dbgr-track-set-debugger "pdb")
  (if pdb-track-mode
      (progn 
	(setq dbgr-track-mode 't)
	(run-mode-hooks (intern (pdb-track-mode-hook))))
    (progn 
      (setq dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-pdb-")
