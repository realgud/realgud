;; show expressions using tooltip

;; Copyright (C) 2015 Free Software Foundation, Inc
;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(require 'load-relative)
(require 'tooltip)

(require-relative-list  '("cmds" "helper")    "realgud-")

(declare-function realgud:cmd-eval   'realgud-cmd)
(declare-function realgud-get-cmdbuf 'realgud-helper)

(defun realgud:tooltip-eval (event)
  "Show tip for identifier or selection under the mouse.
The mouse must either point at an identifier or inside a selected
region for the tip window to be shown.

This function must return nil if it doesn't handle EVENT."
  (interactive "e")
  (let ((process)
	(cmdbuf (realgud-get-cmdbuf))
	(process))
    (when (and (eventp event)
	       cmdbuf
	       (setq process (get-buffer-process cmdbuf))
	       (posn-point (event-end event))
	       )
      (let ((expr (tooltip-expr-to-print event))
	    (original-filter (process-filter process)))
	(when expr
	  (set-process-filter process 'realgud:eval-process-output)
	  (realgud:cmd-eval expr)
	  ))
      )))

(defun realgud:eval-process-output (process output)
  "Process debugger output and show it in a tooltip window."
  (set-process-filter process 'comint-output-filter)
  (with-current-buffer (realgud-get-cmdbuf)
    (goto-char (process-mark process))
    (insert output)
    (set-marker (process-mark process) (point)))
    (setq comint-last-output-start
	  (setq realgud-last-output-start (process-mark process)))

  (tooltip-show (tooltip-strip-prompt process output))
  )

(provide-me "realgud-")
