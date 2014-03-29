;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core")
		       "realgud-")
(require-relative-list '("init") "realgud-nodejs-")

(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud-nodejs-minibuffer-history nil
  "minibuffer history list for the command `nodejs'.")

(easy-mmode-defmap realgud-nodejs-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of nodejs startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun nodejs-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud-nodejs-suggest-invocation
   realgud-nodejs-minibuffer-local-map
   'realgud-nodejs-minibuffer-history
   opt-debugger))

(defun nodejs-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing
- the name of the debugger given (e.g. nodejs) and its arguments - a list of strings
- the script name and its arguments - list of strings
- whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input
  (map 'list 'symbol-name
   '(node --interactive --debugger-port 5858 /tmp nodejs ./gcd.rb a b))

we might return:
   ((node --interactive --debugger-port 5858) (./gcd.rb a b) 't)

NOTE: the above should have each item listed in quotes.
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
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil, nil, nil)
	(list debugger-args script-args annotate-p)
      ;; else
      (progn
	;; Remove "nodejs" from "nodejs --nodejs-options script
	;; --script-options"
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^node$" debugger-name)
	  (message
	   "Expecting debugger name `%s' to be `node'"
	   debugger-name))
	(setq debugger-args (list (pop args)))

	;; Skip to the first non-option argument.
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ;; Options with arguments.
	     ((string-match "^-" arg)
	      (setq pair (realgud-parse-command-arg
			  args nodejs-two-args nodejs-opt-two-args))
	      (nconc debugger-args (car pair))
	      (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	     (t (setq script-name arg)
	      (setq script-args args))
	   )))
      (list debugger-args script-args annotate-p)))))

(defvar nodejs-command-name) ; # To silence Warning: reference to free variable
(defun realgud-nodejs-suggest-invocation (debugger-name)
  "Suggest a nodejs command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation nodejs-command-name
			      realgud-nodejs-minibuffer-history
			      "JavaScript" "\\.js$"))

(defun realgud-nodejs-remove-ansi-shmutz()
  "Remove ASCII escape sequences that node.js 'decorates' in
prompts and interactive output with"
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))
  )

(defun realgud-nodejs-reset ()
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


(defun realgud-nodejs-customize ()
  "Use `customize' to edit the settings of the `nodejs' debugger."
  (interactive)
  (customize-group 'nodejs))

(provide-me "realgud-nodejs-")
