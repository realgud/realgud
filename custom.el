(require 'load-relative)

(defcustom dbgr-populate-common-keys-function
  'dbgr-populate-common-keys-standard
  "The function to call to populate key bindings common to all dbgr windows.
This includes the secondary windows, the debugger shell, and all
Ruby source buffers when the debugger is active.

This variable can be bound to the following:

* nil -- Don't bind any keys.

* `dbgr-populate-common-keys-standard' -- Bind according to a widely used
  debugger convention:

\\{dbgr-example-map-standard}

* `dbgr-populate-common-keys-eclipse' -- Bind according to Eclipse.

\\{dbgr-example-map-eclipse}

* `dbgr-populate-common-keys-netbeans' -- Bind according to NetBeans.

\\{dbgr-example-map-netbeans}

* Any other value is expected to be a callable function that takes one
  argument, the keymap, and populates it with suitable keys."
  :type 'function
  :group 'dbgr)

(defcustom dbgr-key-prefix "\C-x\C-a"
  "Prefix of all GUD commands valid in C buffers."
  :type 'string
  :group 'dbbgr)

(provide-me "dbgr-")
