;;; rbdbgr-track-mode.el --- Ruby "rbdbgr" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))

(require 'load-relative)
(dolist 
    (rel-file 
     '("../dbgr-track-mode" "rbdbgr-core" "rbdbgr-regexp"))
  (require-relative rel-file))

(defvar rbdbgr-pat-hash)
(defvar rbdbgr-track-mode nil
  "Non-nil if using rbdbgr-track mode as a minor mode of some other mode.
Use the command `rbdbgr-track-mode' to toggle or set this variable.")

;; FIXME: DRY below declarations shared in rbdbgr-track-mode by
;; figuring out how to put in a common header.
(declare-function dbgr-track-set-debugger (debugger-name &optional hash))
(declare-function dbgr-track-mode(bool))

(defun rbdbgr-track-mode-body()
  "Called when entering or leaving rbdbgr-track-mode"
  (dbgr-track-set-debugger "rbdbgr" rbdbgr-pat-hash)
  (if rbdbgr-track-mode
      (progn 
 	;; FIXME: until I figure out why this isn't set in the mode
	(local-set-key "\C-c!"  'rbdbgr-goto-dollarbang-traceback-line)
        (local-set-key "\C-ce"  'rbdbgr-goto-traceback-line)
	(dbgr-track-mode 't)
	(run-mode-hooks 'rbdbgr-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
      (local-unset-key "\C-c!")
      (local-unset-key "\C-ce"))
    ))

(defvar rbdbgr-track-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-c !]	'rbdbgr-goto-dollarbang-traceback-line)
    (define-key map [C-c e]	'rbdbgr-goto-traceback-line)
    map)
  "Keymap used in `rbdbgr-track-mode'.")

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

(provide 'rbdbgr-track-mode)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-track.el ends here
