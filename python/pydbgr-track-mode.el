;;; pydbgr-track-mode.el --- Ruby "pydbgr" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list
 '("../dbgr-track-mode" "../dbgr-cmds" "pydbgr-core"))

(defvar pydbgr-pat-hash)
(defvar pydbgr-track-mode nil
  "Non-nil if using pydbgr-track mode as a minor mode of some other mode.
Use the command `pydbgr-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defun pydbgr-track-mode-body()
  "Called when entering or leaving pydbgr-track-mode"
  (dbgr-track-set-debugger "pydbgr")
  (dbgr-define-gdb-like-commands)
  (if pydbgr-track-mode
      (progn 
 	;; FIXME: until I figure out why this isn't set in the mode
        (local-set-key "\C-ce"  'pydbgr-goto-traceback-line)
	(dbgr-track-mode 't)
	(run-mode-hooks 'pydbgr-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
      (local-unset-key "\C-ce")
    )))

(defvar pydbgr-track-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-c e]	'pydbgr-goto-traceback-line)
    map)
  "Keymap used in `pydbgr-track-mode'.")

(define-minor-mode pydbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " pydbgr"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pydbgr
  :keymap pydbgr-track-mode-map
  (pydbgr-track-mode-body)
)

(provide 'pydbgr-track-mode)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; pydbgr-track.el ends here
