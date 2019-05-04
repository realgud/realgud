; Copyright (C) 2010-2011, 2015, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

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
      (realgud-menu-item debugger-map "Go to Command Buffer"
			 'realgud-window-cmd-undisturb-src
			 :enable '(and (realgud-get-process) (not (realgud-cmdbuf?)))
			 :help (documentation 'realgud-window-cmd-undisturb-src)
			 ))

    (define-key debugger-map [source]
      (realgud-menu-item debugger-map "Go to Source Buffer"
			 'realgud-window-src-undisturb-cmd
			 :enable '(and (realgud-get-process) (not (realgud-srcbuf?)))
			 :help (documentation 'realgud-window-src-undisturb-cmd)
			 ))

    (define-key debugger-map [info]
      (realgud-menu-item debugger-map "Debugger Info"
			 'realgud:cmdbuf-info-describe
			 :help (documentation 'realgud:cmdbuf-info-describe)
			 ))

    (define-key debugger-map [backtrace]
      (realgud-menu-item debugger-map "Backtrace" 'realgud:window-bt
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:window-bt)
			 ))

    (define-key debugger-map [breakpoints]
      (realgud-menu-item debugger-map "Breakpoints" 'realgud:window-brkpt
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:window-brkpt)
			 ))

    (define-key debugger-map [arrow3]
      (realgud-menu-item debugger-map "Arrow 3" 'realgud-goto-arrow3
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-goto-arrow3)
			 ))

    (define-key debugger-map [arrow2]
      (realgud-menu-item debugger-map "Arrow 2" 'realgud-goto-arrow2
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-goto-arrow2)
			 ))

    (define-key debugger-map [arrow1]
      (realgud-menu-item debugger-map "Arrow 1" 'realgud-goto-arrow1
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-goto-arrow1)
			 ))

    (define-key debugger-map [break]
	(realgud-menu-item debugger-map "Set Breakpoint" 'realgud:cmd-break
			   :enable '(realgud-get-process)
			   :help (documentation 'realgud:cmd-break)
			   ))

    (define-key debugger-map [continue]
      (realgud-menu-item debugger-map "Continue" 'realgud:cmd-continue
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-continue)
			 ))

    (define-key debugger-map [until]
      (realgud-menu-item debugger-map "Continue to Line" 'realgud:cmd-until
			 :enable '(and (realgud-get-process) realgud-short-key-mode)
			 :help (documentation 'realgud:cmd-until)
			 ))

    (define-key debugger-map [next]
      (realgud-menu-item debugger-map "Next (step through)" 'realgud:cmd-next
			 :enable '(realgud-get-process)
			   :help (documentation 'realgud:cmd-next)
			   ))

    (define-key debugger-map [finish]
      (realgud-menu-item debugger-map "Finish (step out)" 'realgud:cmd-finish
			 :enable '(realgud-get-process)
			   :help (documentation 'realgud:cmd-finish)
			   ))

    (define-key debugger-map [step]
      (realgud-menu-item debugger-map "Step (step into)" 'realgud:cmd-step
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-step)
			 ))

    (define-key debugger-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key debugger-map [down]
      (realgud-menu-item debugger-map "Down Stack" 'realgud:cmd-newer-frame
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-newer-frame)
			 ))

    (define-key debugger-map [up]
      (realgud-menu-item debugger-map "Up Stack" 'realgud:cmd-older-frame
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-older-frame)
			 ))

    (define-key debugger-map [menu-bar debugger line1] '(menu-item "--"))

    (define-key debugger-map [quit]
      (realgud-menu-item debugger-map "Quit" 'realgud:cmd-quit
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-quit)
			 ))

    (define-key debugger-map [restart]
      (realgud-menu-item debugger-map "Restart" 'realgud:cmd-restart
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-restart)
			 ))

    (define-key debugger-map [eval]
      (realgud-menu-item debugger-map "Evaluate region or string" 'realgud:cmd-eval-dwim
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud:cmd-eval-dwim)
			 ))

    (define-key debugger-map [Recenter]
      (realgud-menu-item debugger-map "Recenter" 'realgud-recenter-arrow
			 :enable '(realgud-get-process)
			 :help (documentation 'realgud-recenter-arrow)
			 ))
    ;; Put them in the menu bar:
    (setq menu-bar-final-items (append '(debugger) menu-bar-final-items))
    map
  ))

(provide-me "realgud-")
