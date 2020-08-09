;; Copyright (C) 2010 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

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

(defcustom realgud-immediately-show-all-locals t
  "Immediately expand all values in locals window."
  :type 'boolean
  :group 'realgud)

(defcustom realgud-update-hook nil
  "List of hooks to be run when debugger hits breakpoint"
  :type 'hook
  :group 'realgud)

(provide-me "realgud-")
