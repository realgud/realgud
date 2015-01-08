;; Copyright (C) 2010-2011, 2015 Rocky Bernstein <rocky@gnu.org>
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
the realgud common map contains a binding it is displayed in the
menu. (The common map typically contains function key bindings.)"
  (let ((key-binding (where-is-internal cmd (list common-map) t))
        (hint '()))
    (if key-binding
        (setq hint (list :keys (key-description key-binding))))
    (append (list 'menu-item name cmd)
            hint
            args)))


;; I had a *lot* of trouble with emacs keymaps and duplicate menus.
;; Don't use set-parent-mode on any minor mode.
;; It is possible that realgud-track-mode could be a derived major mode whose parent
;; is comint, but that seems drastic.
;; Instead we take the various minor modes and add a debugger menu to that.
(defun realgud-populate-debugger-menu (map)
  "Populate the 'Debugger' menu inside an existing menu (short-key or track-mode)."
  (let ((debugger-map (make-sparse-keymap "Debugger")))
    (define-key map [menu-bar debugger] (cons "Debugger" debugger-map))
    (define-key debugger-map [command]
      (realgud-menu-item debugger-map "command" 'realgud-window-cmd-undisturb-src
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-window-cmd-undisturb-src)
			 ))

    (define-key debugger-map [source]
      (realgud-menu-item debugger-map "source" 'realgud-window-src-undisturb-cmd
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-window-src-undisturb-cmd)
			 ))

    (define-key debugger-map [backtrace]
      (realgud-menu-item debugger-map "backtrace" 'realgud-window-bt
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-window-bt)
			 ))

    (define-key debugger-map [arrow3]
      (realgud-menu-item debugger-map "arrow 3" 'realgud-goto-arrow3
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-goto-arrow3)
			 ))

    (define-key debugger-map [arrow2]
      (realgud-menu-item debugger-map "arrow 2" 'realgud-goto-arrow2
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-goto-arrow2)
			 ))

    (define-key debugger-map [arrow1]
      (realgud-menu-item debugger-map "arrow 1" 'realgud-goto-arrow1
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-goto-arrow1)
			 ))

    (define-key debugger-map [break]
	(realgud-menu-item debugger-map "Set breakpoint" 'realgud-cmd-break
			   :enable '(realgud-get-process)
			   :help (documentation 'realgud-cmd-break)
			   ))

    (define-key debugger-map [continue]
      (realgud-menu-item debugger-map "continue" 'realgud-cmd-continue
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-continue)
			 ))

    (define-key debugger-map [next]
      (realgud-menu-item debugger-map "next (step through)" 'realgud-cmd-next
			 :enable '(realgud-get-process)
			   :help (documentation 'realgud-cmd-next)
			   ))

    (define-key debugger-map [finish]
      (realgud-menu-item debugger-map "finish (step out)" 'realgud-cmd-finish
			 :enable '(realgud-get-process)
			   :help (documentation 'realgud-cmd-finish)
			   ))

    (define-key debugger-map [step]
      (realgud-menu-item debugger-map "step (step into)" 'realgud-cmd-step
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-step)
			 ))

    (define-key debugger-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key debugger-map [up]
      (realgud-menu-item debugger-map "up" 'realgud-cmd-newer-frame
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-newer-frame)
			 ))

    (define-key debugger-map [down]
      (realgud-menu-item debugger-map "down" 'realgud-cmd-older-frame
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-older-frame)
			 ))

    (define-key debugger-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key debugger-map [quit]
      (realgud-menu-item debugger-map "quit" 'realgud-cmd-quit
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-quit)
			 ))

    (define-key debugger-map [restart]
      (realgud-menu-item debugger-map "restart" 'realgud-cmd-restart
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-restart)
			 ))

    (define-key debugger-map [eval]
      (realgud-menu-item debugger-map "eval" 'realgud-cmd-eval-region
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-cmd-eval-region)
			 ))

    (define-key debugger-map [recenter]
      (realgud-menu-item debugger-map "recenter" 'realgud-recenter-arrow
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-recenter-arrow)
			 ))
    ;; Put them in the menu bar:
    (setq menu-bar-final-items (append '(debugger) menu-bar-final-items))
    map
  ))

(provide-me "realgud-")
