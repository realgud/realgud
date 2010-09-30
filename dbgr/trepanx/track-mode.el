;;; Ruby "trepanx" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/trepanx") "dbgr-init-")
(require-relative-list '("core" "cmds") "trepanx-")

(dbgr-track-mode-vars "trepanx")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY.

(dbgr-populate-common-keys trepanx-track-minor-mode-map)

(define-key trepanx-track-minor-mode-map 
  (kbd "C-c !!") 'trepanx-goto-dollarbang-traceback-line)
(define-key trepanx-track-minor-mode-map 
  (kbd "C-c !b") 'trepanx-goto-traceback-line)

(define-minor-mode trepanx-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepanx"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanx
  :keymap trepanx-track-minor-mode-map
  (dbgr-track-mode-body "trepanx")
)

(provide-me "trepanx-")

;;; Local variables:
;;; eval:(put 'trepan-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; trepanx-track.el ends here
