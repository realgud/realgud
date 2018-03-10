;; Copyright (C) 2015-2017 Free Software Foundation, Inc

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

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core")
		       "realgud-")
(require-relative-list '("init") "realgud:nodejs-")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:nodejs-minibuffer-history nil
  "minibuffer history list for the command `nodejs'.")

(easy-mmode-defmap realgud:nodejs-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of nodejs startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun nodejs-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud:nodejs-suggest-invocation
   realgud:nodejs-minibuffer-local-map
   'realgud:nodejs-minibuffer-history
   opt-debugger))

;;; FIXME: DRY this with other *-parse-cmd-args routines
(defun nodejs-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for the name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing
* the name of the debugger given (e.g. nodejs) and its arguments - a list of strings
* the script name and its arguments - list of strings

For example for the following input:
  (map 'list 'symbol-name
   '(node --interactive --debugger-port 5858 /tmp nodejs ./gcd.js a b))

we might return:
   ((\"node\" \"--interactive\" \"--debugger-port\" \"5858\") nil (\"/tmp/gcd.js\" \"a\" \"b\"))

Note that path elements have been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  node nodejs-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	(node-two-args '("-debugger_port" "C" "D" "i" "l" "m" "-module" "x"))
	;; node doesn't have any optional two-arg options
	(node-opt-two-args '())

	;; One dash is added automatically to the below, so
	;; h is really -h and -debugger_port is really --debugger_port.
	(nodejs-two-args '("-debugger_port"))
	(nodejs-opt-two-args '())

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(interpreter-args '())
	(script-args '())
	)
    (if (not (and args))
	;; Got nothing: return '(nil, nil, nil)
	(list interpreter-args nil script-args)
      ;; else
      (progn
	;; Remove "nodejs" (or "nodemon" or "node") from invocation like:
	;; nodejs --nodejs-options script --script-options
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^node\\(?:js\\|mon\\)?$" debugger-name)
	  (message
	   "Expecting debugger name `%s' to be `node', `nodemon', or `nodejs'"
	   debugger-name))
	(setq interpreter-args (list (pop args)))

	;; Skip to the first non-option argument.
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ((equal "debug" arg)
	      (nconc interpreter-args (list arg))
	      (setq args (cdr args))
	      )

	     ;; Options with arguments.
	     ((string-match "^-" arg)
	      (setq pair (realgud-parse-command-arg
			  args nodejs-two-args nodejs-opt-two-args))
	      (nconc interpreter-args (car pair))
	      (setq args (cadr pair)))
	     ;; Anything else must be the script to debug.
	     (t (setq script-name (realgud:expand-file-name-if-exists arg))
	       (setq script-args (cons script-name (cdr args))))
	     )))
	(list interpreter-args nil script-args)))
    ))

;; To silence Warning: reference to free variable
(defvar realgud:nodejs-command-name)

(defun realgud:nodejs-suggest-invocation (debugger-name)
  "Suggest a nodejs command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation realgud:nodejs-command-name
			      realgud:nodejs-minibuffer-history
			      "js" "\\.js$"))

(defun realgud:nodejs-remove-ansi-shmutz()
  "Remove ASCII escape sequences that node.js 'decorates' in
prompts and interactive output with"
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))
  )

(defun realgud:nodejs-reset ()
  "Nodejs cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (nodejs-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*nodejs-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun nodejs-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'nodejs-debugger-support-minor-mode minor-mode-map-alist)
;; 	  nodejs-debugger-support-minor-mode-map-when-deactive))


(defun realgud:nodejs-customize ()
  "Use `customize' to edit the settings of the `nodejs' debugger."
  (interactive)
  (customize-group 'realgud:nodejs))

(provide-me "realgud:nodejs-")
