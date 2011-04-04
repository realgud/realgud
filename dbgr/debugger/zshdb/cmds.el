;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../../common/send") "dbgr-")

(declare-function dbgr-terminate &optional arg)
(declare-function dbgr-define-gdb-like-commands())

(defun dbgr-define-zshdb-commands ()
  "(Re)define a bunch of zshdb commands"
  ;; zshdb doesn't allow for the more general file:line breakpoint yet.
  (dbgr-define-gdb-like-commands)
  (dbgr-define-command 
      'break "break %l" "\C-b" 
      "Set a breakpoint at the current line" t nil)
  )

(local-set-key "\C-cq" 'dbgr-cmd-quit)


(provide-me "dbgr-zshdb-")
