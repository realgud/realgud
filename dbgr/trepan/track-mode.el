;;; Ruby "trepan" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/trepan") "dbgr-init-")
(require-relative-list '("core" "cmds") "trepan-")

(defvar trepan-pat-hash)
(defvar trepan-track-mode nil
  "Non-nil if using trepan-track mode as a minor mode of some other mode.
Use the command `trepan-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defvar trepan-track-mode-map
  (let ((map dbgr-track-mode-map))
    (define-key map [C-c ! !]	'trepan-goto-dollarbang-backtrace-line)
    (define-key map [C-c ! c]	'trepan-goto-control-frame-line)
    (define-key map [C-c ! b]	'trepan-goto-backtrace-line)
    map)
  "Keymap used in `trepan-track-mode'.")

(defun trepan-track-mode-body()
  "Called when entering or leaving trepan-track-mode. Variable
`trepan-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (dbgr-track-set-debugger "trepan")
  (dbgr-define-gdb-like-commands)
  (dbgr-define-trepan-commands)
  (if trepan-track-mode
      (progn 
	(dbgr-populate-common-keys 
	 (or (current-local-map) (use-local-map trepan-track-mode-map)))
	(dbgr-track-mode 't)
	(run-mode-hooks 'trepan-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
      )))

(define-minor-mode trepan-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepan"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan
  :keymap trepan-track-mode-map
  (trepan-track-mode-body)
)

(provide-me "trepan-")

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; trepan-track.el ends here
