;;; Ruby "rdebug" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/rdebug") "dbgr-init-")
(require-relative-list '("core" "cmds") "rdebug-")

(defvar rdebug-pat-hash)
(defvar rdebug-track-mode nil
  "Non-nil if using rdebug-track mode as a minor mode of some other mode.
Use the command `rdebug-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defun rdebug-track-mode-body()
  "Called when entering or leaving rdebug-track-mode. Variable
`pydbgr-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (dbgr-track-set-debugger "rdebug")
  (dbgr-define-gdb-like-commands)
  ;; (dbgr-define-rdebug-commands)
  (if rdebug-track-mode
      (progn 
 	;; FIXME: until I figure out why this isn't set in the mode
	(local-set-key "\C-c!"  'rdebug-goto-dollarbang-traceback-line)
        (local-set-key "\C-ce"  'rdebug-goto-traceback-line)
	(dbgr-track-mode 't)
	(run-mode-hooks 'rdebug-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
      (local-unset-key "\C-c!")
      (local-unset-key "\C-ce"))
    ))

(defvar rdebug-track-mode-map
  (let ((map dbgr-track-mode-map))
    (define-key map [C-c !]	'rdebug-goto-dollarbang-traceback-line)
    (define-key map [C-c e]	'rdebug-goto-traceback-line)
    map)
  "Keymap used in `rdebug-track-mode'.")

(define-minor-mode rdebug-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " rdebug"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'rdebug
  :keymap rdebug-track-mode-map
  (rdebug-track-mode-body)
)

(provide-me "rdebug-")

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:
