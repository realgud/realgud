;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Ruby "trepanx" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../../common/track-mode" "../../common/cmds" 
			 "../../common/menu") "dbgr-")
(require-relative-list '("core" "cmds" "init") "dbgr-trepanx-")
(require-relative-list '("../../lang/ruby") "dbgr-lang-")

(dbgr-track-mode-vars "trepanx")
(declare-function dbgr-track-mode(bool))

;;; FIXME: The following could be more DRY. Also use parnet-maps.
(dbgr-populate-common-keys trepanx-track-minor-mode-map)
(dbgr-ruby-populate-command-keys trepanx-track-minor-mode-map)

(define-minor-mode trepanx-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepanx"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanx
  :keymap trepanx-track-minor-mode-map
  (dbgr-track-mode-body "trepanx")
)

(provide-me "dbgr-trepanx-")

