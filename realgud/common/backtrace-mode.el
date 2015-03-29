;; Copyright (C) 2015 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

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
;;; Debugger Backtrace buffer mode settings
(require 'load-relative)
(require-relative-list  '("menu" "key") "realgud-")
(require-relative-list  '("buffer/command") "realgud-buffer-")

(declare-function realgud-populate-debugger-menu 'realgud-menu)
(declare-function realgud-populate-common-keys 'realgud-menu)
(declare-function realgud-cmdbuf-pat 'realgud-menu)

(defvar realgud:frames-menu nil
  "Frames menu in Backtrace menu.")

;; (setq realgud:frames-menu
;;       (let ((map (make-sparse-keymap "Goto Specific Frames")))
;; 	(define-key map [frames-menu]
;; 	  (list 'menu-item "Specific Frames" 'realgud:frames-menu))
;; 	(realgud-menu-item map "Frame 1" 'realgud-goto-frame-1)
;; 	(realgud-menu-item map "Frame 2" 'realgud-goto-frame-2)
;; 	(realgud-menu-item map "Frame 3" 'realgud-goto-frame-3)
;; 	)
;;       map)

(defvar realgud-backtrace-mode-map
  (let ((map  (realgud-populate-debugger-menu (make-sparse-keymap))))
    (suppress-keymap map)
    (realgud-populate-common-keys map)
    (define-key map "."       'realgud-backtrace-moveto-frame-selected)
    (define-key map "r"       'realgud:backtrace-init)
    (define-key map [double-mouse-1] 'realgud:follow-event)
    (define-key map [mouse-2] 'realgud:follow-event)
    (define-key map [enter]   'realgud:follow-event)
    (define-key map [mouse-3] 'realgud:follow-event)
    (define-key map [enter]   'realgud:follow-event)
    (define-key map [return]  'realgud:follow-point)
    (define-key map [up]      'realgud-backtrace-moveto-frame-prev)
    (define-key map [down]    'realgud-backtrace-moveto-frame-next)
    (define-key map "l"       'realgud-recenter-arrow)

    (define-key map [frames-menu]
      (list 'menu-item "Specific Frames" 'realgud:frames-menu))

    ;; FIXME: these can go to a common routine. See also shortkey.el and
    ;; key.el
    (define-key map "<"       'realgud:cmd-newer-frame)
    (define-key map ">"       'realgud:cmd-older-frame)
    (define-key map "d"       'realgud:cmd-newer-frame)
    (define-key map "u"       'realgud:cmd-older-frame)
    (define-key map "q"       'realgud:cmd-quit)
    (define-key map "C"       'realgud-window-cmd-undisturb-src)
    (define-key map "F"       'realgud:window-bt)
    (define-key map "I"       'realgud:cmdbuf-info-describe)
    (define-key map "S"       'realgud-window-src-undisturb-cmd)

    (define-key map "n"       'realgud-backtrace-moveto-frame-next)
    (define-key map "p"       'realgud-backtrace-moveto-frame-prev)
    (define-key map "0"       'realgud-goto-frame-n)
    (define-key map "1"       'realgud-goto-frame-n)
    (define-key map "2"       'realgud-goto-frame-n)
    (define-key map "3"       'realgud-goto-frame-n)
    (define-key map "4"       'realgud-goto-frame-n)
    (define-key map "5"       'realgud-goto-frame-n)
    (define-key map "6"       'realgud-goto-frame-n)
    (define-key map "7"       'realgud-goto-frame-n)
    (define-key map "8"       'realgud-goto-frame-n)
    (define-key map "9"       'realgud-goto-frame-n)

    ;; --------------------
    ;; The "Stack window" submenu.
    ;; (let ((submenu realgud:frames-menu))
    ;;   (define-key-after map [menu-bar debugger stack]
    ;;     (cons "Stack window" submenu)
    ;;     'placeholder))
    map)
  "Keymap to navigate realgud stack frames.

\\{realgud-backtrace-mode-map}")

(defun realgud-backtrace-mode (&optional cmdbuf)
  "Major mode for displaying the stack frames.
\\{realgud-frames-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'realgud-backtrace-mode)
  (setq mode-name "Realgud Stack Frames")
  ;; (set (make-local-variable 'realgud-secondary-buffer) t)
  (setq mode-line-process 'realgud-mode-line-process)
  (use-local-map realgud-backtrace-mode-map)

  ;; FIXME: make buffer specific
  (if cmdbuf
      (let* ((font-lock-keywords
	      (with-current-buffer cmdbuf
		(realgud-cmdbuf-pat "font-lock-keywords"))))
	(if font-lock-keywords
	    (set (make-local-variable 'font-lock-defaults)
		 (list font-lock-keywords)))
	))
  ;; (run-mode-hooks 'realgud-backtrace-mode-hook)
  )

(provide-me "realgud-")
