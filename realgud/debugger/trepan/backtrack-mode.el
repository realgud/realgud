;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>

;;; Mode for parsing various kinds of backtraces found in the Ruby

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/backtrack-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud-trepan-")
(require-relative-list '("../../lang/ruby") "realgud-lang-")

(realgud-backtrack-mode-vars "trepan")
(set-keymap-parent trepan-backtrack-mode-map realgud-backtrack-mode-map)

(declare-function realgud-backtrack-mode         'realgud-common-backtrack-mode)
(declare-function realgud-backtrack-set-debugger 'realgud-common-backtrack-mode)

(defun realgud-trepan-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "control-frame"))

(realgud-ruby-populate-command-keys trepan-backtrack-mode-map)
(define-key trepan-backtrack-mode-map
  (kbd "C-c !c") 'realgud-trepan-goto-control-frame-line)

(define-minor-mode trepan-backtrack-mode
  "Minor mode for tracking ruby debugging inside a file which may not have process shell."
  :init-value nil
  ;; :lighter " trepan"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepan
  :keymap trepan-backtrack-mode-map

  (realgud-backtrack-set-debugger "trepan")
  (if trepan-backtrack-mode
      (progn
	(realgud-backtrack-mode 't)
	(run-mode-hooks (intern (trepan-backtrack-mode-hook))))
    (progn
      (realgud-backtrack-mode nil)
      ))
)

(defun trepan-backtrack-mode-hook()
  (if trepan-backtrack-mode
      (progn
	(use-local-map trepan-backtrack-mode-map)
	(message "using trepan mode map")
	)
    (message "trepan backtrack-mode-hook disable called"))
)

(provide-me "realgud-trepan-")
