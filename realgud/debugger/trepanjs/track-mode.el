;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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

;;; "trepanjs" Debugger tracking a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 "../../common/utils"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepanjs-")
;; (require-relative-list '("../../lang/js") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-goto-line-for-pt 'realgud-track-mode)
(declare-function realgud:remove-ansi-schmutz 'realgud:utils)

(realgud-track-mode-vars "trepanjs")

(define-key realgud-track-mode-map
  (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
(define-key realgud-track-mode-map
  (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)

(declare-function realgud:ruby-populate-command-keys 'realgud-lang-ruby)

(define-key trepanjs-track-mode-map
  (kbd "C-c !s") 'realgud:js-goto-syntax-error-line)

(defun trepanjs-track-mode-hook()
  (if trepanjs-track-mode
      (progn
	(use-local-map trepanjs-track-mode-map)
	(realgud:remove-ansi-schmutz)
	(message "using trepanjs mode map")
	)
    (message "trepanjs track-mode-hook disable called"))
)

(define-minor-mode trepanjs-track-mode
  "Minor mode for tracking trepanjs source locations inside a process shell via realgud. trepanjs is a Ruby debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepanjs-track-mode-map}
"
  :init-value nil
  ;; :lighter " trepanjs"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepanjs
  :keymap trepanjs-track-mode-map
  (realgud:track-set-debugger "trepanjs")
  (if trepanjs-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (trepanjs-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud:trepanjs-")
