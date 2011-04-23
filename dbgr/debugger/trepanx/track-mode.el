;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Ruby "trepanx" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-trepanx-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(dbgr-track-mode-vars "trepanx")

(declare-function dbgr-track-mode(bool))

(dbgr-ruby-populate-command-keys trepanx-track-mode-map)

(define-key trepanx-track-mode-map 
  (kbd "C-c !x") 'dbgr-rubinius-goto-Xagent-backtrace-line)
(define-key trepanx-track-mode-map 
  (kbd "C-c !!") 'dbgr-ruby-goto-dollar-bang-line)

(defun trepanx-track-mode-hook()
  (use-local-map trepanx-track-mode-map)
  (message "trepanx track-mode-hook called")
)
(define-minor-mode trepanx-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepanx"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanx
  :keymap trepanx-track-mode-map

  (dbgr-track-set-debugger "trepanx")
  (if trepanx-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (trepanx-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-trepanx-")

