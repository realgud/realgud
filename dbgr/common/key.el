(require 'load-relative)

(defcustom dbgr-populate-common-fn-keys-function
  'dbgr-populate-common-fn-keys-standard
  "The function to call to populate key bindings common to all dbgr windows.
This includes the secondary windows, the debugger shell, and all
Ruby source buffers when the debugger is active.

This variable can be bound to the following:

* nil -- Don't bind any keys.

* `dbgr-populate-common-fn-keys-standard' -- Bind the function
  keys according to a widely used debugger convention:

\\{dbgr-example-map-standard}

* `dbgr-populate-common-fn-keys-eclipse' -- Bind according to Eclipse.

\\{dbgr-example-map-eclipse}

* `dbgr-populate-common-fn-keys-netbeans' -- Bind according to NetBeans.

\\{dbgr-example-map-netbeans}

* Any other value is expected to be a callable function that takes one
  argument, the keymap, and populates it with suitable keys."
  :type 'function
  :group 'dbgr)

;; -------------------------------------------------------------------
;; Key bindings
;;

(defun dbgr-populate-common-fn-keys-standard (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{dbgr-example-map-standard}"
  (define-key map [f5]    'dbgr-continue)
  (define-key map [S-f5]  'dbgr-cmd-quit)
  ;; (define-key map [f9]    'dbgr-toggle-source-breakpoint)
  (define-key map [f9]    'dbgr-cmd-break)
  ;; (define-key map [C-f9]  'dbgr-toggle-source-breakpoint-enabled)
  (define-key map [f10]   'dbgr-cmd-next)
  (define-key map [f11]   'dbgr-cmd-step)
  (define-key map [S-f11] 'dbgr-cmd-finish)
  (define-key map [M-down]    'dbgr-track-hist-newer)
  (define-key map [M-kp-2]    'dbgr-track-hist-newer)
  (define-key map [M-up]      'dbgr-track-hist-older)
  (define-key map [M-kp-8]    'dbgr-track-hist-older)
  (define-key map [M-kp-up]   'dbgr-track-hist-older)
  (define-key map [M-kp-down] 'dbgr-track-hist-newer)
  (define-key map [M-print]   'dbgr-track-hist-older)
  (define-key map [M-S-down]  'dbgr-track-hist-newest)
  (define-key map [M-S-up]    'dbgr-track-hist-oldest)
  (define-key map "\C-c " 'dbgr-cmd-break)
  )

;; TODO: add eclipse, and netbeans

(defun dbgr-populate-common-keys (map)
  "Define the keys that are used by all debugger buffers, including 
source-code buffers

The variable `dbgr-populate-common-fn-keys-function' controls the layout."
  (define-key map "\C-x\C-a\C-q" 'dbgr-short-key-mode)
  (if dbgr-populate-common-fn-keys-function
      (funcall dbgr-populate-common-fn-keys-function map))
  (dbgr-populate-common-fn-keys-standard map)
)

(defun dbgr-populate-src-buffer-map-plain (map)
  "Bind ordinary text characters used in debugger source-code buffers.

This does not touch change menus; for that see `dbgr-populate-debugger-menu'.
Nor does it touch prefix keys; for that see `dbgr-populate-keys-standard'"
  ;; Keys to view other buffers.
  (let ((prefix-map (make-sparse-keymap)))
    (define-key map "b" 'dbgr-cmd-break)
    (define-key map " " 'dbgr-cmd-step)
    (define-key map "<" 'dbgr-cmd-newer-frame)
    (define-key map ">" 'dbgr-cmd-older-frame)
    (define-key map "f" 'dbgr-cmd-finish)
    (define-key map "n" 'dbgr-cmd-next)
    (define-key map "q" 'dbgr-cmd-quit)
    (define-key map "r" 'dbgr-cmd-restart)
    (define-key map "R" 'dbgr-cmd-restart)
    (define-key map "s" 'dbgr-cmd-step)
    (define-key map [M-down]    'dbgr-track-hist-newer)
    (define-key map [M-kp-2]    'dbgr-track-hist-newer)
    (define-key map [M-up]      'dbgr-track-hist-older)
    (define-key map [M-kp-8]    'dbgr-track-hist-older)
    (define-key map [M-kp-up]   'dbgr-track-hist-older)
    (define-key map [M-kp-down] 'dbgr-track-hist-newer)
    (define-key map [M-print]   'dbgr-track-hist-older)
    (define-key map [M-S-down]  'dbgr-track-hist-newest)
    (define-key map [M-S-up]    'dbgr-track-hist-oldest)
    ))

(provide-me "dbgr-")

