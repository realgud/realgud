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

(defun dbgr-gdb-track-mode-body()
  "Called when entering or leaving gdb-track-mode. Variable
`gdb-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (dbgr-track-set-debugger "gdb")
  (dbgr-define-gdb-like-commands)
  (if gdb-track-mode
      (progn 
 	;; FIXME: until I figure out why this isn't set in the mode
        (local-set-key "\C-ce"  'gdb-goto-traceback-line)
	(dbgr-track-mode 't)
	(run-mode-hooks 'dbgr-gdb-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
      (local-unset-key "\C-ce")
    )))

(defvar dbgr-gdb-track-mode-map
  (let ((map dbgr-track-mode-map))
    map)
  "Keymap used in `gdb-track-mode'.")

(define-minor-mode dbgr-gdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " gdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'dbgr-gdb
  :keymap dbgr-gdb-track-mode-map
  (dbgr-gdb-track-mode-body)
)

(provide-me "gdb-")
