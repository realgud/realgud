;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Ruby "trepan" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-trepan-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(dbgr-track-mode-vars "trepan")

(declare-function dbgr-track-mode(bool))

(defun dbgr-trepan-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt pt "control-frame"))

(defun dbgr-trepan-goto-syntax-error-line (pt)
  "Display the location mentioned in a Syntax error line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt pt "syntax-error"))

(dbgr-ruby-populate-command-keys trepan-track-mode-map)

(define-key trepan-track-mode-map 
  (kbd "C-c !c") 'dbgr-trepan-goto-control-frame-line)
(define-key trepan-track-mode-map 
  (kbd "C-c !s") 'dbgr-trepan-goto-syntax-error-line)

(defun trepan-track-mode-hook()
  (if trepan-track-mode
      (progn
	(use-local-map trepan-track-mode-map)
	(message "using trepan mode map")
	)
    (message "trepan track-mode-hook disable called"))
)

(define-minor-mode trepan-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepan"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan
  :keymap trepan-track-mode-map

  (dbgr-track-set-debugger "trepan")
  (if trepan-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (trepan-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-trepan-")
