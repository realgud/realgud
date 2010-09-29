;;; gdb tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list
 '("../common/track-mode" "../common/cmds") "dbgr-")
(require-relative "gdb-core")

(defvar gdb-pat-hash)
(defvar gdb-track-mode nil
  "Non-nil if using gdb-track mode as a minor mode of some other mode.
Use the command `dbgr-gdb-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defvar dbgr-gdb-track-minor-mode-map (make-sparse-keymap)
  "Keymap for dbgr-gdb track minor mode.")
(dbgr-populate-common-keys dbgr-gdb-track-minor-mode-map)
(define-key dbgr-gdb-track-minor-mode-map 
  (kbd "C-c !b") 'dbgr-gdb-goto-traceback-line)

(defun dbgr-gdb-track-mode-body()
  "Called when entering or leaving gdb-track-mode. Variable
`gdb-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (if gdb-track-mode
      (progn 
	(dbgr-track-set-debugger "gdb")
	(dbgr-define-gdb-like-commands) ;; FIXME: unless already defined
	(dbgr-track-mode 't)
	(run-mode-hooks 'dbgr-gdb-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
    )))

(define-minor-mode dbgr-gdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " gdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'dbgr-gdb
  :keymap dbgr-gdb-track-minor-mode-map
  (dbgr-gdb-track-mode-body)
)

(provide-me "gdb-")
