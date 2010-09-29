;;; Python "pydbgr" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds"
			 "../common/menu") "dbgr-")
(require-relative-list '("core") "pydbgr-")

(defvar pydbgr-pat-hash)
(defvar pydbgr-track-mode nil
  "Non-nil if using pydbgr-track mode as a minor mode of some other mode.
Use the command `pydbgr-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defvar pydbgr-track-minor-mode-map (make-sparse-keymap)
  "Keymap for pydbgr track minor mode.")
(dbgr-populate-common-keys pydbgr-track-minor-mode-map)
(define-key pydbgr-track-minor-mode-map 
  (kbd "C-c !b") 'pydbgr-goto-traceback-line)

(defun pydbgr-track-mode-body()
  "Called when entering or leaving pydbgr-track-mode. Variable
`pydbgr-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (if pydbgr-track-mode
      (progn 
	(dbgr-track-set-debugger "pydbgr")
	(dbgr-define-gdb-like-commands) ;; FIXME: unless already defined
	(dbgr-track-mode 't)
	(run-mode-hooks 'pydbgr-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
    )))

(define-minor-mode pydbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " pydbgr"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pydbgr
  :keymap pydbgr-track-minor-mode-map
  (pydbgr-track-mode-body)
)

(provide-me "pydbgr-")
