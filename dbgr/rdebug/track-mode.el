;;; Ruby "rdebug" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/rdebug") "dbgr-init-")
(require-relative-list '("core" "cmds") "rdebug-")

(dbgr-track-mode-vars "rdebug")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY.

(dbgr-populate-common-keys rdebug-track-minor-mode-map)
(define-key rdebug-track-minor-mode-map 
  (kbd "C-c !b") 'rdebug-goto-traceback-line)

(define-minor-mode rdebug-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " rdebug"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'rdebug
  :keymap rdebug-track-minor-mode-map
  (dbgr-track-mode-body "rdebug")
)

(provide-me "rdebug-")

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:
