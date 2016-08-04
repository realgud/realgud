;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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
;;; process-command buffer things

;;; A mode based off of org mode to show debugger information

(eval-when-compile (require 'cl-lib))
(require 'org)

(require 'load-relative)

(cl-defstruct realgud-backtrace-info
  "debugger object/structure specific to debugger info."
  (cmdbuf    nil)  ;; buffer of the associated debugger process
)

;; Can't load because this causes a cyclic dependency on
;; buffer/commands via cmd and buffer/commands uses us.
;; (require-relative-list  '("menu") "realgud-")

;; FIXME: full definition is in menu.
(defvar realgud:info-mode-map)

(defcustom realgud:info-mode-hook '()
  "Hook for customizing realgud info mode."
  :type 'hook
  :group 'realgud)


(define-derived-mode realgud:info-mode org-mode "Debugger Info"
  "Major mode for interacting realgud debugger information."
  (use-local-map realgud:info-mode-map)
  (setq buffer-read-only t)
  )

;; FIXME:
;; (defvar realgud:info-mode-map
;;   (realgud-populate-debugger-menu
;; 	      (make-sparse-keymap "Debugger")))

;; (define-key realgud:info-mode-map [menu-bar debugger]
;;   (cons "Debugger" (realgud-populate-debugger-menu
;; 	      (make-sparse-keymap "Debugger"))))

;; (define-key realgud:info-mode-map [menu-bar debugger backtrace]
;;   '("Backtrace" . realgud:window-bt))
;;(define-key realgud:info-mode-map  [menu-bar debugger command]
;;  '("Go to Command Buffer" . realgud-window-cmd-undisturb-src))
;;(define-key realgud:info-mode-map [menu-bar debugger source]
;;  '("Go to Source Buffer" . realgud-window-src-undisturb-cmd))

(provide-me "realgud-buffer-")
