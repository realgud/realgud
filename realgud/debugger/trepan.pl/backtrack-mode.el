;; Copyright (C) 2011, 2014, 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Mode for parsing various kinds of backtraces found in Perl

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
                         "../../common/track-mode"
			 "../../common/backtrack-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepanpl-")
(require-relative-list '("../../lang/perl") "realgud-lang-")

(declare-function realgud-goto-line-for-pt
		  'realgud-track-mode)
(declare-function realgud-backtrack-set-debugger
		  'realgud-common-backtrack-mode)
(declare-function realgud-perl-populate-command-keys
		  'realgud-lang-perl)

(realgud-backtrack-mode-vars "trepanpl")
(set-keymap-parent trepanpl-backtrack-mode-map realgud-backtrack-mode-map)

(declare-function realgud-backtrack-mode(bool))

(defun realgud:trepanpl-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "control-frame"))

(realgud-perl-populate-command-keys trepanpl-backtrack-mode-map)
(define-key trepanpl-backtrack-mode-map
  (kbd "C-c !c") 'realgud:trepanpl-goto-control-frame-line)

(define-minor-mode trepanpl-backtrack-mode
  "Minor mode for tracking ruby debugging inside a file which may not have process shell."
  :init-value nil
  ;; :lighter " trepanpl"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepanpl
  :keymap trepanpl-backtrack-mode-map

  (realgud-backtrack-set-debugger "trepan.pl")
  (if trepanpl-backtrack-mode
      (progn
	(realgud-backtrack-mode 't)
	(run-mode-hooks (intern (trepanpl-backtrack-mode-hook))))
    (progn
      (realgud-backtrack-mode nil)
      ))
)

(defun trepanpl-backtrack-mode-hook()
  (if trepanpl-backtrack-mode
      (progn
	(use-local-map trepanpl-backtrack-mode-map)
	(message "using trepanpl mode map")
	)
    (message "trepan.pl backtrack-mode-hook disable called"))
)

(provide-me "realgud:trepanpl-")
