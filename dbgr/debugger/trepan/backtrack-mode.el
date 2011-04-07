;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>

;;; Mode for parsing various kinds of backtraces found in the Ruby

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/backtrack-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-trepan-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(dbgr-backtrack-mode-vars "trepan")
(set-keymap-parent trepan-backtrack-mode-map dbgr-backtrack-mode-map)

(declare-function dbgr-backtrack-mode(bool))

(defun dbgr-trepan-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt pt "control-frame"))

(dbgr-ruby-populate-command-keys trepan-backtrack-mode-map)
(define-key trepan-backtrack-mode-map 
  (kbd "C-c !c") 'dbgr-trepan-goto-control-frame-line)

(define-minor-mode trepan-backtrack-mode
  "Minor mode for tracking ruby debugging inside a file which may not have process shell."
  :init-value nil
  ;; :lighter " trepan"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan
  :keymap trepan-backtrack-mode-map

  (dbgr-backtrack-set-debugger "trepan")
  (if trepan-backtrack-mode
      (progn 
	(dbgr-backtrack-mode 't)
	(run-mode-hooks (intern (trepan-backtrack-mode-hook))))
    (progn 
      (dbgr-backtrack-mode nil)
      ))
)

(defun trepan-backtrack-mode-hook()
  (if trepan-backtrack-mode
      (progn
	(use-local-map trepan-backtrack-mode-map)
	(message "using trepan mode map")
	)
    (message "trepan backtrack-mode-hook disable called"))
)

(provide-me "dbgr-trepan-")
