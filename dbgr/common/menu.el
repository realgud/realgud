;; -------------------------------------------------------------------
;; Menu support.
;;

(require 'load-relative)

;; We want the doc strings from gdb-like commands for our help
;; menus.
(require-relative-list '("cmds") "dbgr-")
(dbgr-define-gdb-like-commands)

;; Note: We want the key binding to show in the menu. However, our
;; situation is a little bit complex:
;;
;; 1) We want the binding of the `common' man (i.e. the function key
;;    the user has selected.)
;;
;; 2) We want this even when the menu is disabled and the key isn't
;;    bound, typically when the debugger isn't running.
;;
;; This has been solved by setting up an explicit ":keys" properly.
(defun dbgr-menu-item (common-map name cmd &rest args)
  "Return a menu item entry with the correct key bindings.

A command can be bound to a number of different key sequences. If
the dbgr common map contains a binding it is displayed in the
menu. (The common map typically contains function key bindings.)"
  (let ((key-binding (where-is-internal cmd (list common-map) t))
        (hint '()))
    (if key-binding
        (setq hint (list :keys (key-description key-binding))))
    (append (list 'menu-item name cmd)
            hint
            args)))


;; Note, we re-populate the menus of the different minor and major
;; modes. The reason is that Emacs caches the key bindings, which
;; means that wrong ones are shown when buffers are changed.

;; Remember, all menu items are added in the reverse order!

(defun dbgr-populate-debugger-menu (parent-map)
  "Populate the 'Dbgr' menu by creating a keymap whose parent is PARENT-MAP."
  (let ((menu-map (make-sparse-keymap)))

    (define-key menu-map [menu-bar debugger] (cons "Dbgr" menu-map))
    (set-keymap-parent parent-map menu-map)

    ;; FIXME: Dry define-key menu-map ... with a macro.

    ;; (define-key menu [break-delete]
    ;;   (dbgr-menu-item menu-map "Enable/disable breakpoint"
    ;;                     'dbgr-toggle-source-breakpoint-enabled
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key menu [break]
    ;;   (dbgr-menu-item menu-map "Toggle breakpoint"
    ;;                     'dbgr-toggle-source-breakpoint
    ;;                     :enable '(dbgr-get-process)))

    (define-key menu-map [break]
      (dbgr-menu-item menu-map "Set breakpoint" 'dbgr-cmd-break
                        :enable '(dbgr-get-process)
			:help (documentation 'dbgr-cmd-continue)
			))

    (define-key menu-map [cont]
      (dbgr-menu-item menu-map "continue" 'dbgr-cmd-continue
                        :enable '(dbgr-get-process)
			:help (documentation 'dbgr-cmd-continue)
			))

    (define-key menu-map [finish]
      (dbgr-menu-item menu-map "finish (step out)" 'dbgr-cmd-finish
                        :enable '(dbgr-get-process)
			:help (documentation 'dbgr-cmd-finish)
			))

    (define-key menu-map [next]
      (dbgr-menu-item menu-map "next (step over)" 'dbgr-cmd-next
                        :enable '(dbgr-get-process)
			:help (documentation 'dbgr-cmd-next)
			))

    (define-key menu-map [step]
      (dbgr-menu-item menu-map "step (step into)" 'dbgr-cmd-step
                        :enable '(dbgr-get-process)
			:help (documentation 'dbgr-cmd-step)
			))

    (define-key menu-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key menu-map [stop]
      (dbgr-menu-item menu-map "quit" 'dbgr-cmd-quit
		      :enable '(dbgr-get-process)
		      :help (documentation 'dbgr-cmd-quit)
		      ))

    (define-key menu-map [start]
      (dbgr-menu-item menu-map "restart" 'dbgr-cmd-restart
		      :enable '(dbgr-get-process)
		      :help (documentation 'dbgr-cmd-restart)
		      ))

    (define-key menu-map [menu-bar debugger line2] '(menu-item "--"))

    ;; ;; --------------------
    ;; ;; The "Options" submenu.

    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key menu [options] (cons "Options" submenu)))

    ;; (define-key map [menu-bar debugger options customize]
    ;;   (dbgr-menu-item menu-map
    ;;                     "Customize Dbgr" 'dbgr-customize))

    ;; (define-key map [menu-bar debugger options line1] '(menu-item "--"))



    ;; ----------------
    ;; The "short key" toggle.

    (define-key menu-map [menu-bar debugger options short-key-mode]
      (dbgr-menu-item menu-map
                        "Short keys in source" 'dbgr-short-key-mode
                        :button
			:help "Toggle single characters as debugger commands"
                        '(:toggle
                          . dbgr-short-key-mode)))

    menu-map))

(provide-me "dbgr-")
