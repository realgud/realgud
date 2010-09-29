;;; Ruby "rbdbgr" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/rbdbgr") "dbgr-init-")
(require-relative-list '("core" "cmds") "rbdbgr-")

(defvar rbdbgr-pat-hash)
(defvar rbdbgr-track-mode nil
  "Non-nil if using rbdbgr-track mode as a minor mode of some other mode.
Use the command `rbdbgr-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defvar rbdbgr-track-mode-map
  (let ((map dbgr-track-mode-map))
    (define-key map [C-c ! !]	'rbdbgr-goto-dollarbang-backtrace-line)
    (define-key map [C-c ! c]	'rbdbgr-goto-control-frame-line)
    (define-key map [C-c ! b]	'rbdbgr-goto-backtrace-line)
    map)
  "Keymap used in `rbdbgr-track-mode'.")

(defun rbdbgr-track-mode-body()
  "Called when entering or leaving rbdbgr-track-mode. Variable
`rbdbgr-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (dbgr-track-set-debugger "rbdbgr")
  (dbgr-define-gdb-like-commands)
  (dbgr-define-rbdbgr-commands)
  (if rbdbgr-track-mode
      (progn 
	(dbgr-populate-common-keys 
	 (or (current-local-map) (use-local-map (make-sparse-keymap))))
	(dbgr-track-mode 't)
	(run-mode-hooks 'rbdbgr-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
    )))

(define-minor-mode rbdbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " rbdbgr"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'rbdbgr
  :keymap rbdbgr-track-mode-map
  (rbdbgr-track-mode-body)
)

(provide-me "rbdbgr-")

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-track.el ends here
