;;; Python "pydbgr" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds"
			 "../common/menu") "dbgr-")
(require-relative-list '("core" "cmds") "pydbgr-")

(dbgr-track-mode-vars "pydbgr")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY.

(dbgr-populate-common-keys pydbgr-track-minor-mode-map)
(define-key pydbgr-track-minor-mode-map 
  (kbd "C-c !b") 'pydbgr-goto-traceback-line)

(define-minor-mode pydbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " pydbgr"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pydbgr
  :keymap pydbgr-track-minor-mode-map
  (dbgr-track-mode-body "pydbgr")
)

(provide-me "pydbgr-")
