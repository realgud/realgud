;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)

(defcustom realgud-key-prefix "\C-x\C-a"
  "Prefix of all realgud commands valid in source buffers."
  :type 'string
  :group 'realgud)

(defcustom realgud-srcbuf-lock t
  "Set source buffers read-only when the debugger is active.
A setting of `nil` allows editing, but Short-Key-mode use may inhibit this."
  :type 'boolean
  :group 'realgud)

(provide-me "realgud-")
