;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.1
;; Keywords: internal
;; URL: http://github.com/rocky/emacs-load-relative
;; Compatibility: GNU Emacs 24.x

;; Copyright (C) 2015, 2017, 2019 Free Software Foundation, Inc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(provide 'key)
;;; key.el ends here

(require 'load-relative)
(require-relative "custom" nil "realgud-")

(defcustom realgud-populate-common-fn-keys-function
  'realgud-populate-common-fn-keys-standard
  "The function to call to populate key bindings common to all realgud windows.
This includes the secondary windows, the debugger shell, and all
realgud source buffers when the debugger is active.

This variable can be bound to the following:

* `realgud-populate-common-fn-keys-none' -- Don't bind any keys.
* `realgud-populate-common-fn-keys-standard' -- Bind the function
* Any other value is expected to be a callable function that takes one
  argument, the keymap, and populates it with suitable keys."
  :type 'function
  :group 'realgud)

;; -------------------------------------------------------------------
;; Key bindings
;;

(defun realgud-populate-common-fn-keys-standard (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map [f5]    'realgud:cmd-continue)
  (define-key map [S-f5]  'realgud:cmd-quit)
  ;; (define-key map [f9]    'realgud-toggle-source-breakpoint)
  (define-key map [f9]    'realgud:cmd-break)
  ;; (define-key map [C-f9]  'realgud-toggle-source-breakpoint-enabled)
  (define-key map [f10]   'realgud:cmd-next)
  (define-key map [f11]   'realgud:cmd-step)
  (define-key map [S-f11] 'realgud:cmd-finish)
  (define-key map [M-down]    'realgud-track-hist-newer)
  (define-key map [A-down]    'realgud-track-hist-newer)
  (define-key map [M-kp-2]    'realgud-track-hist-newer)
  (define-key map [M-up]      'realgud-track-hist-older)
  (define-key map [A-up]      'realgud-track-hist-older)
  (define-key map [M-kp-8]    'realgud-track-hist-older)
  (define-key map [M-kp-up]   'realgud-track-hist-older)
  (define-key map [M-kp-down] 'realgud-track-hist-newer)
  (define-key map [M-print]   'realgud-track-hist-older)
  (define-key map [M-S-down]  'realgud-track-hist-newest)
  (define-key map [M-S-up]    'realgud-track-hist-oldest)
  (define-key map "\C-c " 'realgud:cmd-break)
  )

(defun realgud-populate-common-fn-keys-none (&optional map)
  "Do not any debugger function keys"
  )

;; TODO: add eclipse, and netbeans

(defun realgud-populate-common-keys (map)
  "Define the keys that are used by all debugger buffers, including
source-code buffers

The variable `realgud-populate-common-fn-keys-function' controls the layout."
  (define-key map "\C-x\C-a\C-q" 'realgud-short-key-mode)
  (if realgud-populate-common-fn-keys-function
      (funcall realgud-populate-common-fn-keys-function map)))

(defun realgud-populate-src-buffer-map-plain (map)
  "Bind ordinary text characters used in debugger source-code buffers.

This does not touch change menus; for that see `realgud-populate-debugger-menu'.
Nor does it touch prefix keys; for that see `realgud-populate-keys-standard'"
  ;; Common Debugger functions
  (let ((prefix-map (make-sparse-keymap)))
    (define-key map "b" 'realgud:cmd-break)
    (define-key map "D" 'realgud:cmd-delete)
    (define-key map "X" 'realgud:cmd-clear)
    (define-key map "-" 'realgud:cmd-disable)
    (define-key map "+" 'realgud:cmd-enable)
    (define-key map "T" 'realgud:cmd-backtrace)
    (define-key map [delete] 'realgud:cmd-delete)
    (define-key map [enter] 'realgud:cmd-repeat-last)
    (define-key map (kbd "RET") 'realgud:cmd-repeat-last)
    (define-key map " " 'realgud:cmd-step)
    (define-key map "f" 'realgud:cmd-finish)
    (define-key map "n" 'realgud:cmd-next)
    (define-key map "q" 'realgud:cmd-quit)
    (define-key map "k" 'realgud:cmd-kill)
    (define-key map "r" 'realgud:cmd-restart)
    (define-key map "R" 'realgud:cmd-restart)
    (define-key map "s" 'realgud:cmd-step)
    (define-key map "!" 'realgud:cmd-shell)

    ;; FIXME: these can go to a common routine. See also shortkey.el
    ;; and backtrace-mode.el
    (define-key map "<" 'realgud:cmd-newer-frame)
    (define-key map ">" 'realgud:cmd-older-frame)
    (define-key map "d" 'realgud:cmd-newer-frame)
    (define-key map "B" 'realgud:window-brkpt)
    (define-key map "L" 'realgud:window-locals)
    (define-key map "u" 'realgud:cmd-older-frame)
    (define-key map "C" 'realgud-window-cmd-undisturb-src)
    (define-key map "F" 'realgud:window-bt)
    (define-key map "Q" 'realgud:cmd-terminate)
    (define-key map "S" 'realgud-window-src-undisturb-cmd)
    (define-key map "U" 'realgud:cmd-until)
    (define-key map "h" 'realgud:cmd-until-here)

    (define-key map [M-down]    'realgud-track-hist-newer)
    (define-key map [M-kp-2]    'realgud-track-hist-newer)
    (define-key map [M-up]      'realgud-track-hist-older)
    (define-key map [M-kp-8]    'realgud-track-hist-older)
    (define-key map [M-kp-up]   'realgud-track-hist-older)
    (define-key map [M-kp-down] 'realgud-track-hist-newer)
    (define-key map [M-print]   'realgud-track-hist-older)
    (define-key map [M-S-down]  'realgud-track-hist-newest)
    (define-key map [M-S-up]    'realgud-track-hist-oldest)
    ))

(provide-me "realgud-")
