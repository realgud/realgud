;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
;; -------------------------------------------------------------------
;; Menu support.
;;

(require 'load-relative)

;; We want the doc strings from gdb-like commands for our help
;; menus.
(require-relative-list '("cmds") "realgud-")

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
(defun realgud-menu-item (common-map name cmd &rest args)
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

(defun realgud-populate-debugger-menu (parent-map)
  "Populate the 'Debugger' menu by creating a keymap whose parent is PARENT-MAP."
  (let ((menu-map (make-sparse-keymap))
	)

    (define-key menu-map [menu-bar debugger] (cons "Debugger" menu-map))
    (set-keymap-parent parent-map menu-map)

    ;; FIXME: Dry define-key menu-map ... with a macro.

    ;; (define-key menu [break-delete]
    ;;   (realgud-menu-item menu-map "Enable/disable breakpoint"
    ;;                     'realgud-toggle-source-breakpoint-enabled
    ;;                     :enable '(realgud-get-process)))

    ;; (define-key menu [break]
    ;;   (realgud-menu-item menu-map "Toggle breakpoint"
    ;;                     'realgud-toggle-source-breakpoint
    ;;                     :enable '(realgud-get-process)))

    (define-key menu-map [command]
      (realgud-menu-item menu-map "command" 'realgud-window-cmd-undisturb-src
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-window-cmd-undisturb-src)
		      ))

    (define-key menu-map [source]
      (realgud-menu-item menu-map "source" 'realgud-window-src-undisturb-cmd
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-window-src-undisturb-cmd)
		      ))

    (define-key menu-map [backtrace]
      (realgud-menu-item menu-map "backtrace" 'realgud-window-bt
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-window-bt)
		      ))

    (define-key menu-map [arrow3]
      (realgud-menu-item menu-map "arrow 3" 'realgud-goto-arrow3
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-goto-arrow3)
		      ))

    (define-key menu-map [arrow2]
      (realgud-menu-item menu-map "arrow 2" 'realgud-goto-arrow2
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-goto-arrow2)
		      ))

    (define-key menu-map [arrow1]
      (realgud-menu-item menu-map "arrow 1" 'realgud-goto-arrow1
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-goto-arrow1)
		      ))

    (define-key menu-map [break]
      (realgud-menu-item menu-map "Set breakpoint" 'realgud-cmd-break
                        :enable '(realgud-get-process)
			:help (documentation 'realgud-cmd-break)
			))

    (define-key menu-map [continue]
      (realgud-menu-item menu-map "continue" 'realgud-cmd-continue
                        :enable '(realgud-get-process)
			:help (documentation 'realgud-cmd-continue)
			))

    (define-key menu-map [next]
      (realgud-menu-item menu-map "next (step through)" 'realgud-cmd-next
                        :enable '(realgud-get-process)
			:help (documentation 'realgud-cmd-next)
			))

    (define-key menu-map [finish]
      (realgud-menu-item menu-map "finish (step out)" 'realgud-cmd-finish
                        :enable '(realgud-get-process)
			:help (documentation 'realgud-cmd-finish)
			))

    (define-key menu-map [step]
      (realgud-menu-item menu-map "step (step into)" 'realgud-cmd-step
                        :enable '(realgud-get-process)
			:help (documentation 'realgud-cmd-step)
			))

    (define-key menu-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key menu-map [up]
      (realgud-menu-item menu-map "up" 'realgud-cmd-newer-frame
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-cmd-newer-frame)
		      ))

    (define-key menu-map [down]
      (realgud-menu-item menu-map "down" 'realgud-cmd-older-frame
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-cmd-older-frame)
		      ))

    (define-key menu-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key menu-map [quit]
      (realgud-menu-item menu-map "quit" 'realgud-cmd-quit
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-cmd-quit)
		      ))

    (define-key menu-map [restart]
      (realgud-menu-item menu-map "restart" 'realgud-cmd-restart
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-cmd-restart)
		      ))

    (define-key menu-map [eval]
      (realgud-menu-item menu-map "eval" 'realgud-cmd-eval-region
                        :enable '(realgud-get-process)
			:help (documentation 'realgud-cmd-eval-region)
			))

    (define-key menu-map [recenter]
      (realgud-menu-item menu-map "recenter" 'realgud-recenter-arrow
		      :enable '(realgud-get-process)
		      :help (documentation 'realgud-recenter-arrow)
		      ))

    (define-key menu-map [menu-bar debugger line2] '(menu-item "--"))

    ;; ;; --------------------
    ;; ;; The "Options" submenu.

    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key menu [options] (cons "Options" submenu)))

    ;; (define-key map [menu-bar debugger options customize]
    ;;   (realgud-menu-item menu-map
    ;;                     "Customize Dbgr" 'realgud-customize))

    ;; (define-key map [menu-bar debugger options line1] '(menu-item "--"))



    ;; ----------------
    ;; The "short key" toggle.

    (define-key menu-map [menu-bar debugger options short-key-mode]
      (realgud-menu-item menu-map
                        "Short keys in source" 'realgud-short-key-mode
                        :button
			:help "Toggle single characters as debugger commands"
                        '(:toggle
                          . realgud-short-key-mode)))

    menu-map))

(provide-me "realgud-")
