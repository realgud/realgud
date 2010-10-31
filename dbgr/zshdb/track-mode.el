;;; Ruby "zshdb" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode"
			 "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("core" "cmds" "init") "dbgr-zshdb-")

(dbgr-track-mode-vars "zshdb")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY.

(dbgr-populate-common-keys zshdb-track-minor-mode-map)
(define-key zshdb-track-minor-mode-map 
  (kbd "C-c !!") 'zshdb-goto-dollarbang-traceback-line)
(define-key zshdb-track-minor-mode-map 
  (kbd "C-c !b") 'zshdb-goto-backtrace-line)
(define-key zshdb-track-minor-mode-map 
  (kbd "C-c !c") 'zshdb-goto-control-frame-line)
(define-key zshdb-track-minor-mode-map 
  (kbd "C-c !c") 'zshdb-goto-control-frame-line)

(define-minor-mode zshdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " zshdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'zshdb
  :keymap zshdb-track-minor-mode-map
  (dbgr-track-mode-body "zshdb")
)

(provide-me "dbgr-zshdb-")

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; zshdb-track.el ends here
