;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Ruby "rdebug" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-rdebug-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(dbgr-track-mode-vars "rdebug")

(declare-function dbgr-track-mode(bool))

(dbgr-ruby-populate-command-keys rdebug-track-mode-map)

(defun rdebug-track-mode-hook()
  (use-local-map rdebug-track-mode-map)
  (message "rdebug track-mode-hook called")
)

(define-minor-mode rdebug-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " rdebug"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'rdebug
  :keymap rdebug-track-mode-map
  (dbgr-track-set-debugger "rdebug")
  (if rdebug-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (rdebug-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-rdebug-")
