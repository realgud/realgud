;;; gdb tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list
 '("../common/track-mode" "../common/cmds") "dbgr-")
(require-relative-list '("core") "dbgr-gdb-")

(dbgr-track-mode-vars "dbgr-gdb")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY.

(dbgr-populate-common-keys dbgr-gdb-track-minor-mode-map)
(define-key dbgr-gdb-track-minor-mode-map 
  (kbd "C-c !b") 'dbgr-gdb-goto-traceback-line)

(define-minor-mode dbgr-gdb-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " gdb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'dbgr-gdb
  :keymap dbgr-gdb-track-minor-mode-map
  (dbgr-track-mode-body "dbgr-gdb")
)

(provide-me "dbgr-gdb-")
