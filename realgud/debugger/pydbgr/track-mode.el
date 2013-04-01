;;; Copyright (C) 2010, 2012 Rocky Bernstein <rocky@gnu.org>
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
		       "realgud-")
(require-relative-list '("core" "init") "realgud-pydbgr-")

(realgud-track-mode-vars "pydbgr")

(declare-function realgud-track-mode(bool))

(realgud-python-populate-command-keys pydbgr-track-mode-map)

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
  ;; :lighter " pydbgr"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pydbgr
  :keymap pydbgr-track-mode-map
  (realgud-track-set-debugger "pydbgr")
  (if pydbgr-track-mode
      (progn
	(setq realgud-track-mode 't)
	(realgud-track-mode-setup 't)
	(run-mode-hooks (intern (pydbgr-track-mode-hook))))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-pydbgr-")
