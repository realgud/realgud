;;; pydbgr-track-mode.el --- Ruby "pydbgr" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(dolist 
    (rel-file 
     '("../dbgr-track-mode" "pydbgr-core"))
  (require-relative rel-file))

(defvar pydbgr-track-mode nil
  "Non-nil if using pydbgr-track mode as a minor mode of some other mode.
Use the command `pydbgr-track-mode' to toggle or set this variable.")

;; FIXME: DRY below declarations shared in rbdbgr-track-mode by
;; figuring out how to put in a common header.
(declare-function dbgr-track-set-debugger (debugger-name))
(declare-function dbgr-track-mode(bool))

(defun pydbgr-track-mode-body()
  "Called when entering or leaving pydbgr-track-mode"
  (dbgr-track-set-debugger "pydbgr")
  (if pydbgr-track-mode
      (progn 
        (local-set-key "\C-ce"  'pydbgr-goto-traceback-line)
	;; FIXME: (pydbgr-track-mode 't) has problems
	;; until I figure out this out...
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

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'pydbgr-track-mode)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; pydbgr-track.el ends here
