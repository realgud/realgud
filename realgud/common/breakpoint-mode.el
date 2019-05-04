;; Copyright (C) 2019 Free Software Foundation, Inc

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
;;; Debugger Breakpoint buffer mode settings
(require 'load-relative)
(require-relative-list  '("menu" "key") "realgud-")
(require-relative-list  '("buffer/command") "realgud-buffer-")

(declare-function realgud-populate-debugger-menu 'realgud-menu)
(declare-function realgud-populate-common-keys 'realgud-menu)
(declare-function realgud-cmdbuf-pat 'realgud-menu)

(defvar realgud:breakpoints-menu nil
  "menu in Breakpoints buffer.")


(defvar realgud-breakpoint-mode-map
  (let ((map  (realgud-populate-debugger-menu (make-sparse-keymap))))
    (suppress-keymap map)
    (realgud-populate-common-keys map)
    (define-key map "r"       'realgud:breakpoint-init)
    (define-key map [double-mouse-1] 'realgud:follow-event)
    (define-key map [mouse-2] 'realgud:follow-event)
    (define-key map [enter]   'realgud:follow-event)
    (define-key map [mouse-3] 'realgud:follow-event)
    (define-key map [enter]   'realgud:follow-event)
    (define-key map [return]  'realgud:follow-point)
    (define-key map "l"       'realgud-recenter-arrow)

    (define-key map [frames-menu]
      (list 'menu-item "Specific Frames" 'realgud:frames-menu))

    ;; FIXME: these can go to a common routine. See also shortkey.el and
    ;; key.el
    (define-key map "q"       'realgud:cmd-quit)
    (define-key map "C"       'realgud-window-cmd-undisturb-src)
    (define-key map "B"       'realgud:window-brkpt)
    (define-key map "I"       'realgud:cmdbuf-info-describe)
    (define-key map "S"       'realgud-window-src-undisturb-cmd)

    map)
  "Keymap to navigate realgud breakpoints.

\\{realgud-breakpoint-mode-map}")

(defun realgud-breakpoint-mode (&optional cmdbuf)
  "Major mode for displaying the stack frames.
\\{realgud-frames-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'realgud-breakpoint-mode)
  (setq mode-name "Realgud Breakpoints")
  ;; (set (make-local-variable 'realgud-secondary-buffer) t)
  (setq mode-line-process 'realgud-mode-line-process)
  (use-local-map realgud-breakpoint-mode-map)

  ;; FIXME: make buffer specific
  (if cmdbuf
      (let* ((font-lock-breakpoint-keywords
	      (with-current-buffer cmdbuf
		(realgud-cmdbuf-pat "font-lock-breakpoint-keywords"))))
	(if font-lock-breakpoint-keywords
	    (set (make-local-variable 'font-lock-defaults)
		 (list font-lock-breakpoint-keywords)))
	))
  ;; (run-mode-hooks 'realgud-breakpoint-mode-hook)
  )

(provide-me "realgud-")
