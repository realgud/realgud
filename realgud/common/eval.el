;; show expressions using tooltip

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.1
;; Keywords: internal
;; URL: http://github.com/rocky/emacs-load-relative
;; Compatibility: GNU Emacs 25.x

;; Copyright (C) 2015 Free Software Foundation, Inc

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

(require 'tooltip)
(require 'ansi-color)
(require 'load-relative)

(require-relative-list '("cmds" "helper" "utils")  "realgud-")
(require-relative-list '("buffer/command")         "realgud-buffer-")

(declare-function realgud:cmd-eval   'realgud-cmd)
(declare-function realgud-get-cmdbuf 'realgud-helper)
(declare-function realgud-cmdbuf-pat 'realgud-send)
(declare-function realgud:strip      'realgud-utils)

(make-variable-buffer-local
 (defvar realgud:process-filter-save nil
   "realgud saves/restores the previous process filter here"))

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
      (let ((expr (tooltip-expr-to-print event)))
	(when expr
	  (with-current-buffer cmdbuf
	    (setq realgud:process-filter-save (process-filter process))
	    (set-process-filter process 'realgud:eval-process-output))
	  (realgud:cmd-eval expr)
	  ))
      )))

(defun realgud:eval-process-output (process output-str)
  "Process debugger output and show it in a tooltip window."
  (set-process-filter process
		      (or realgud:process-filter-save 'comint-output-filter))
  (with-current-buffer (realgud-get-cmdbuf)
    (goto-char (process-mark process))
    (setq comint-last-input-end (process-mark process))
    (insert output-str)
    (set-marker (process-mark process) (point)))
    (setq comint-last-output-start
	  (setq realgud-last-output-start (process-mark process)))

  (tooltip-show (realgud:eval-strip process output-str))
  )

(defun realgud:eval-strip-default(prompt-regexp output-str)
  (realgud:strip
   (ansi-color-filter-apply
    (if (string-match prompt-regexp output-str)
	(substring output-str 0 (match-beginning 0))
      output-str))))


(defun realgud:eval-strip(process output-str)
  "Return OUTPUT-STR with any prompt of PROCESS stripped from its end."
  (save-match-data
    (with-current-buffer (process-buffer process)
      (let* ((prompt-pat (realgud-cmdbuf-pat "prompt"))
	     (prompt-regexp (realgud-loc-pat-regexp prompt-pat))
	     (eval-filter (realgud-sget 'cmdbuf-info 'callback-eval-filter))
	     )
	(if eval-filter
	    (funcall eval-filter output-str)
	  (realgud:eval-strip-default prompt-regexp output-str))
	))))

(provide-me "realgud-")
