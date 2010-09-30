;;; Ruby "trepan" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode"
			 "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/trepan") "dbgr-init-")
(require-relative-list '("core" "cmds") "trepan-")

(dbgr-track-mode-vars "trepan")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY.

(dbgr-populate-common-keys trepan-track-minor-mode-map)
(define-key trepan-track-minor-mode-map 
  (kbd "C-c !!") 'trepan-goto-dollarbang-traceback-line)
(define-key trepan-track-minor-mode-map 
  (kbd "C-c !b") 'trepan-goto-backtrace-line)
(define-key trepan-track-minor-mode-map 
  (kbd "C-c !c") 'trepan-goto-control-frame-line)
(define-key trepan-track-minor-mode-map 
  (kbd "C-c !c") 'trepan-goto-control-frame-line)

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

(provide-me "trepan-")

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; trepan-track.el ends here
