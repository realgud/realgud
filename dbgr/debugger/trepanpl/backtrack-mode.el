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
(require-relative-list '("core" "init") "dbgr-trepanpl-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(dbgr-backtrack-mode-vars "trepanpl")
(set-keymap-parent trepanpl-backtrack-mode-map dbgr-backtrack-mode-map)

(declare-function dbgr-backtrack-mode(bool))

(defun dbgr-trepanpl-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt pt "control-frame"))

(dbgr-ruby-populate-command-keys trepanpl-backtrack-mode-map)
(define-key trepanpl-backtrack-mode-map 
  (kbd "C-c !c") 'dbgr-trepanpl-goto-control-frame-line)

(define-minor-mode trepanpl-backtrack-mode
  "Minor mode for tracking ruby debugging inside a file which may not have process shell."
  :init-value nil
  ;; :lighter " trepanpl"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanpl
  :keymap trepanpl-backtrack-mode-map

  (dbgr-backtrack-set-debugger "trepanpl")
  (if trepanpl-backtrack-mode
      (progn 
	(dbgr-backtrack-mode 't)
	(run-mode-hooks (intern (trepanpl-backtrack-mode-hook))))
    (progn 
      (dbgr-backtrack-mode nil)
      ))
)

(defun trepanpl-backtrack-mode-hook()
  (if trepanpl-backtrack-mode
      (progn
	(use-local-map trepanpl-backtrack-mode-map)
	(message "using trepanpl mode map")
	)
    (message "trepanpl backtrack-mode-hook disable called"))
)

(provide-me "dbgr-trepanpl-")
