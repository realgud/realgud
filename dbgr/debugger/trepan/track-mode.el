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
(require-relative-list '("core" "cmds" "init") "dbgr-trepan-")

(dbgr-track-mode-vars "trepan")
(declare-function dbgr-track-mode(bool))

(defun dbgr-trepan-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-pat-line-for-pt pt "control-frame"))

(dbgr-populate-common-keys trepan-track-minor-mode-map)
(dbgr-ruby-populate-command-keys trepan-track-minor-mode-map)

(define-key trepan-track-minor-mode-map 
  (kbd "C-c !c") 'dbgr-trepan-goto-control-frame-line)

(defun trepan-track-mode-hook()
  (use-local-map trepan-track-minor-mode-map)
  (message "trepan track-mode-hook called")
)

(define-minor-mode trepan-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepan"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan
  :keymap trepan-track-minor-mode-map
  (dbgr-track-mode-body "trepan")
)

(provide-me "dbgr-trepan-")
