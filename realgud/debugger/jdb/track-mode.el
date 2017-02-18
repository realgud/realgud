;; Copyright (C) 2015-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Java "jdb" Debugger tracking a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:jdb-")
(require-relative-list '("../../lang/java") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-goto-line-for-pt 'realgud-track-mode)
(declare-function realgud-java-populate-command-keys 'realgud-lang-java)

(realgud-track-mode-vars "realgud:jdb")
;;(defvaralias 'jdb-short-key-mode-map 'realgud:jdb-short-key-mode-map)
;;(defvaralias 'jdb-track-mode         'realgud:track-mode)

(realgud-java-populate-command-keys realgud:jdb-track-mode-map)


(define-key realgud-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key realgud-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(defun realgud:jdb-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "control-frame"))

(defun realgud:jdb-goto-syntax-error-line (pt)
  "Display the location mentioned in a Syntax error line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "syntax-error"))

(define-key realgud:jdb-track-mode-map
  (kbd "C-c !c") 'realgud:jdb-goto-control-frame-line)
(define-key realgud:jdb-track-mode-map
  (kbd "C-c !s") 'realgud:jdb-goto-syntax-error-line)

(defun realgud:jdb-track-mode-hook()
  (if realgud:jdb-track-mode
      (progn
	(use-local-map realgud:jdb-track-mode-map)
	(message "using realgud:jdb-track-mode-map"))
    ;; else
    (progn
      (setq realgud-track-mode nil)
      ))
)

(define-minor-mode realgud:jdb-track-mode
  "Minor mode for tracking jdb source locations inside a process shell via realgud. jdb is a Ruby debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{realgud:jdb-track-mode-map}
"
  :init-value nil
  ;; :lighter " jdb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:jdb
  :keymap realgud:jdb-track-mode-map
  (realgud:track-set-debugger "jdb")
  (if realgud:jdb-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(realgud:jdb-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:jdb-")
