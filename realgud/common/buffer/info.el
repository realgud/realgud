;;; Copyright (C) 2015 Rocky Bernstein <rocky@gnu.org>
;;;
;;; A mode based off of org mode to show debugger information

(eval-when-compile (require 'cl))
(require 'org)

(require 'load-relative)

(defstruct realgud-backtrace-info
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
  (setq buffer-read-only 't)
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
