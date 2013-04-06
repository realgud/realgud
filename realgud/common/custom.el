;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)

(defcustom realgud-key-prefix "\C-x\C-a"
  "Prefix of all Dbgr commands valid in source buffers."
  :type 'string
  :group 'realgud)

(provide-me "realgud-")
