;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Perl trepanning Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-trepanpl-")

(dbgr-track-mode-vars "dbgr-trepanpl")

(declare-function dbgr-track-mode(bool))

(defun dbgr-trepanpl-goto-syntax-error-line (pt)
  "Display the location mentioned in a Syntax error line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt pt "syntax-error"))

(define-key dbgr-trepanpl-track-mode-map 
  (kbd "C-c !s") 'dbgr-trepanpl-goto-syntax-error-line)

(defun dbgr-trepanpl-track-mode-hook()
  (if dbgr-trepanpl-track-mode
      (progn
	(use-local-map dbgr-trepanpl-track-mode-map)
	(message "using trepanpl mode map")
	)
    (message "trepanpl track-mode-hook disable called"))
)

(define-minor-mode dbgr-trepanpl-track-mode
  "Minor mode for tracking Perl debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepanpl"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanpl
  :keymap dbgr-trepanpl-track-mode-map

  (dbgr-track-set-debugger "trepanpl")
  (if dbgr-trepanpl-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (dbgr-trepanpl-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-trepanpl-")
