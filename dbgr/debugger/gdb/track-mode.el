;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; gdb tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-gdb-")

(dbgr-track-mode-vars "dbgr-gdb")

(declare-function dbgr-track-mode(bool))

(define-key dbgr-gdb-track-mode-map 
  (kbd "C-c !b") 'dbgr-goto-debugger-backtrace-line)

(defun dbgr-gdb-track-mode-hook()
  (use-local-map dbgr-gdb-track-mode-map)
  (message "dbgr-gdb track-mode-hook called")
)

(define-minor-mode dbgr-gdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " gdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'dbgr-gdb
  :keymap dbgr-gdb-track-mode-map
  (if dbgr-gdb-track-mode
      (progn 
	(dbgr-track-set-debugger "gdb")
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (dbgr-gdb-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-gdb-")
