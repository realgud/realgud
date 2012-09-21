;;; Copyright (C) 2010, 2012 Rocky Bernstein <rocky@gnu.org>
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
  (if rdebug-track-mode
      (progn
	(use-local-map trepan-track-mode-map)
	(message "using trepan mode map")
	)
    (message "trepan track-mode-hook disable called"))
)

(define-minor-mode rdebug-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " rdebug"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'rdebug
  :keymap rdebug-track-mode-map
  (rdebug-track-mode-internal rdebug-track-mode)
)

;; Broken out as a function for debugging
(defun rdebug-track-mode-internal (&optional arg)
  (dbgr-track-set-debugger "rdebug")
  (if rdebug-track-mode
      (progn 
	(setq dbgr-track-mode 't)
	(dbgr-track-mode-setup 't)
	(trepan-track-mode-hook))
    (progn 
      (setq dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-rdebug-")
