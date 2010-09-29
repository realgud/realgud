;;; Ruby "trepanx" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/trepanx") "dbgr-init-")
(require-relative-list '("core" "cmds") "trepanx-")

(defvar trepanx-pat-hash)
(defvar trepanx-track-mode nil
  "Non-nil if using trepanx-track mode as a minor mode of some other mode.
Use the command `trepanx-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defvar trepanx-track-minor-mode-map (make-sparse-keymap)
  "Keymap used in `trepanx-track-mode'.")
(dbgr-populate-common-keys trepanx-track-minor-mode-map)

(define-key trepanx-track-minor-mode-map 
  (kbd "C-c !!") 'trepanx-goto-dollarbang-traceback-line)
(define-key trepanx-track-minor-mode-map 
  (kbd "C-c !b") 'trepanx-goto-traceback-line)

(defun trepanx-track-mode-body()
  "Called when entering or leaving trepanx-track-mode. Variable
`trepanx-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (dbgr-define-gdb-like-commands)
  (dbgr-define-trepanx-commands)
  (if trepanx-track-mode
      (progn 
	(dbgr-track-set-debugger "trepanx")
	(dbgr-define-gdb-like-commands) ;; FIXME: unless already defined
	(dbgr-track-mode 't)
	(run-mode-hooks 'trepanx-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
    )))

(define-minor-mode trepanx-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepanx"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanx
  :keymap trepanx-track-minor-mode-map
  (trepanx-track-mode-body)
)

(provide-me "trepanx-")

;;; Local variables:
;;; eval:(put 'trepan-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; trepanx-track.el ends here
