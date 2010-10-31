;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)

(defcustom dbgr-key-prefix "\C-x\C-a"
  "Prefix of all Dbgr commands valid in source buffers."
  :type 'string
  :group 'dbgr)

(provide-me "dbgr-")
