;; Copyright (C) 2013-2014, 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/lang"
			 "../../common/core") "realgud-")
(require-relative-list '("init") "realgud:gub-")

(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud-lang-mode?         'realgud-lang)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:gub-minibuffer-history nil
  "minibuffer history list for the command `gub'.")

(easy-mmode-defmap gub-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun gub-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'gub-suggest-invocation
   gub-minibuffer-local-map
   'realgud:gub-minibuffer-history
   opt-debugger))

(defun gub-parse-cmd-args (orig-args)
  "Parse command line ARGS for the name of script to debug and its args.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing

- the command processor (e.g. gub.sh) and it's arguments if any - a list of strings
For example for the following input
  \'./gub.sh --gub=\"-I\" -- ./gcd.go a b\'

we might return:
   (gub (\"-gub=-I\") (./gcd.rb a b))

NOTE: the above should have each item listed in quotes.
"

  (let (
	(args orig-args)
	(interp-regexp ".*\\(^gub\.sh\\|tortoise\\)$")

	;; Things returned
	(gub-name "gub.sh")
	(gub-args '())
	(go-prog-and-args '())
	)

    (if (not (and args))
	;; Got nothing
	(list gub-name gub-args go-prog-and-args)
      ;; else
      ;; Strip off "gub.sh"
      (when (string-match interp-regexp
			   (file-name-nondirectory (car args)))
	(setq gub-name (pop args))
	)

      ;; parse options
      (while args
	(let ((arg (pop args)))
	  (cond
	   ((string-match "^-[-]?gub=" arg)
	    (setq gub-args (nconc gub-args (list arg))))

	   ((string-match "^-run" arg)
	    (setq gub-args (nconc gub-args (list arg))))

	   ((string-match "^-interp=SS" arg)
	    (setq gub-args (nconc gub-args (list arg))))

	   ((equal arg "--")) ;; Ignore

	   ;; Anything else add to gub-args
	   ('t (setq go-prog-and-args (nconc go-prog-and-args (list arg))))
	   ))))
      (list gub-name gub-args go-prog-and-args)
    ))

(defconst realgud:gub-auto-suffix-regexp
  "\\.go$"
  "Go file suffix"
)

(defun gub-suggest-file-priority(filename)
  (let ((priority 2)
	(is-not-directory)
	)
    (if (realgud-lang-mode? filename "go")
	(progn
	  (if (string-match realgud:gub-auto-suffix-regexp filename)
	      (setq priority 5)
	    (setq priority 7))
	  ))
    priority
    )
)

;; To silence Warning: reference to free variable
(defvar realgud:gub-command-name)

(defun gub-suggest-invocation (debugger-name)
  "Suggest a command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or debugger-name realgud:gub-command-name)
			      realgud:gub-minibuffer-history
			      "go" "\\.go$"))

;; Convert a command line as would be typed normally to run a script
;; into one that invokes an Emacs-enabled debugging session.
;; "--debugger" in inserted as the first switch.

(defun realgud:gub-massage-args (command-line)
  (let* ((new-args (list "--debugger"))
	 (args (split-string-and-unquote command-line))
	 (program (car args))
	 (seen-e nil)
	 (shift (lambda ()
	 	  (setq new-args (cons (car args) new-args))
	 	  (setq args (cdr args)))))

    ;; Pass all switches and -e scripts through.
    (while (and args
    		(string-match "^-" (car args))
    		(not (equal "-" (car args)))
    		(not (equal "--" (car args))))
      (funcall shift))

    (if (or (not args)
    	    (string-match "^-" (car args)))
    	(error "Can't use stdin as the script to debug"))
    ;; This is the program name.
    (funcall shift)

    (while args
      (funcall shift))

    (nreverse new-args)
    )
  )

(defun gub-reset ()
  "Gub cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (gub-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*gub-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun gub-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'gub-debugger-support-minor-mode minor-mode-map-alist)
;; 	  gub-debugger-support-minor-mode-map-when-deactive))


(defun realgud:gub-customize ()
  "Use `customize' to edit the settings of the `gub' debugger."
  (interactive)
  (customize-group 'realgud:gub))

(provide-me "realgud:gub-")
