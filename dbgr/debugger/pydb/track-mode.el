;;; Copyright (C) 2010, 2012 Rocky Bernstein <rocky@gnu.org>
;;; Python "pydb" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
                         "../../common/cmds" 
                         "../../common/menu"
                         "../../common/track"
                         "../../common/track-mode"
                         ) 
                       "dbgr-")
(require-relative-list '("core" "init") "dbgr-pydb-")

(dbgr-track-mode-vars "pydb")

(declare-function dbgr-track-mode(bool))

(dbgr-python-populate-command-keys pydb-track-mode-map)

(defun pydb-track-mode-hook()
  (if pydb-track-mode
      (progn
        (use-local-map pydb-track-mode-map)
        (message "using pydb mode map")
        )
    (message "pydb track-mode-hook disable called")
    )
)

(define-minor-mode pydb-track-mode
  "Minor mode for tracking Pydb debugging inside a process shell."
  :init-value nil
  ;; :lighter " pydb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'pydb
  :keymap pydb-track-mode-map
  (dbgr-track-set-debugger "pydb")
  (if pydb-track-mode
      (progn 
        (setq dbgr-track-mode 't)
        (dbgr-track-mode-setup 't)
        (pydb-track-mode-hook))
    (progn 
      (setq dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-pydb-")
