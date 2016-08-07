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
;;; nodejs tracking a comint buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 "../../common/utils"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:nodejs-")
;; (require-relative-list '("../../lang/js") "realgud-lang-")

(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:remove-ansi-schmutz 'realgud:utils)

(realgud-track-mode-vars "nodejs")

(declare-function realgud-track-mode(bool))

(defun nodejs-track-mode-hook()
  (if nodejs-track-mode
      (progn
	(use-local-map nodejs-track-mode-map)
	(realgud:remove-ansi-schmutz)
	(message "using nodejs mode map")
	)
    (message "nodejs track-mode-hook disable called"))
)

(define-minor-mode nodejs-track-mode
  "Minor mode for tracking nodejs source locations inside a nodejs shell via realgud.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{nodejs-track-mode-map}"
  :init-value nil
  ;; :lighter " nodejs"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:nodejs

  :keymap nodejs-track-mode-map

  (realgud:track-set-debugger "nodejs")
  (realgud:nodejs-track-mode-internal)
)

(defun realgud:nodejs-track-mode-internal (&optional arg)
  (realgud:track-set-debugger "nodejs")
  (if nodejs-track-mode
      (progn
        (realgud-track-mode-setup 't)
        (nodejs-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
  )

;; ;; Debugger commands that nodejs doesn't have
;; (define-key nodejs-track-mode-map
;;   [remap realgud:cmd-newer-frame] 'undefined)
;; (define-key nodejs-track-mode-map
;;   [remap realgud:cmd-older-frame] 'undefined)
(define-key nodejs-short-key-mode-map
  [remap realgud:cmd-step] 'realgud:cmd-step-no-arg)
(define-key nodejs-short-key-mode-map
  [remap realgud:cmd-step] 'realgud:cmd-step-no-arg)
(define-key nodejs-short-key-mode-map
  [remap realgud:cmd-next] 'realgud:cmd-next-no-arg)

(provide-me "realgud:nodejs-")
