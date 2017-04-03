
;; Copyright (C) 2015-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Felipe Lema <felipelema@mortemale.org>

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

;;; Code:

(require 'load-relative)
(require-relative-list '("../../common/track"
			 "../../common/core"
			 "../../common/lang")
		       "realgud-")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-lang-mode? 'realgud-lang)
(declare-function realgud-parse-command-arg 'realgud-core)
(declare-function realgud-query-cmdline 'realgud-core)

(defvar realgud:undodb-gdb-minibuffer-history nil
  "Minibuffer history list for the command `undodb-gdb'.")

(easy-mmode-defmap realgud:undodb-gdb-minibuffer-local-map
                   '(("\C-i" . comint-dynamic-complete-filename))
                   "Keymap for minibuffer prompting of gud startup command."
                   :inherit minibuffer-local-map)

(defun realgud:undodb-gdb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud:undodb-gdb-suggest-invocation
   realgud:undodb-gdb-minibuffer-local-map
   'realgud:undodb-gdb-minibuffer-history
   opt-debugger))

(defun realgud:undodb-gdb-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for annotate level & name of script to debug.

ORIG_ARGS should contain a tokenized list of the command line to run.

We return the a list containing
* the name of the debugger given (e.g. undodb-gdb) and its arguments -
  a list of strings
* nil (a placeholder in other routines of this ilk for a debugger
* the script name and its arguments - list of strings
* whether the annotate or Emacs option was given
  ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input
  (map 'list 'symbol-name
   '(undodb-gdb --tty /dev/pts/1 -cd ~ --emacs ./gcd.py a b))

we might return:
   ((\"undodb-gdb\" \"--tty\" \"/dev/pts/1\"
    \"-cd\" \"home/rocky\' \"--emacs\") nil \"(/tmp/gcd.py a b\") 't\")

Note that path elements have been expanded via `expand-file-name'."

  ;; Parse the following kind of pattern:
  ;;  undodb-gdb undodb-gdb-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from

	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(undodb-gdb-two-args '("x" "-command" "b" "-exec"
                               "cd" "-pid"  "-core" "-directory"
                               "-annotate"
                               "i" "-interpundodb-gdber"
                               "se" "-symbols" "-tty"))
	;; undodb-gdb doesn't optionsl 2-arg options.
	(undodb-gdb-opt-two-args '())

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil nil nil nil)
	(list debugger-args nil script-args annotate-p)
      ;; else
      (progn

	;; Remove "undodb-gdb" from "undodb-gdb --gdb-options script
	;; --script-options"
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^undodb-gdb.*" debugger-name)
	  (message
	   "Expecting debugger name `%s' to be `undodb-gdb'"
	   debugger-name))
	(setq debugger-args (list (pop args)))

	;; Skip to the first non-option argument.
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ;; Annotation or emacs option with level number.
	     ((or (member arg '("--annotate" "-A"))
                 (equal arg "--emacs"))
	      (setq annotate-p t)
	      (nconc debugger-args (list (pop args) (pop args))))
	     ;; Combined annotation and level option.
	     ((string-match "^--annotate=[0-9]" arg)
	      (nconc debugger-args (list (pop args) (pop args)) )
	      (setq annotate-p t))
	     ((string-match "^--interpreter=" arg)
	      (warn "realgud doesn't support the --interpreter option; option ignored")
	      (setq args (cdr args)))
	     ((equal "-i" arg)
	      (warn "realgud doesn't support the -i option; option ignored")
	      (setq args (cddr args)))
	     ;; path-argument ooptions
	     ((member arg '("-cd" ))
	      (setq arg (pop args))
	      (nconc debugger-args
		     (list arg (realgud:expand-file-name-if-exists
				(pop args)))))
	     ;; Options with arguments.
	     ((string-match "^-" arg)
	      (setq pair (realgud-parse-command-arg
                          args undodb-gdb-two-args undodb-gdb-opt-two-args))
              (nconc debugger-args (car pair))
	      (setq args (cadr pair)))
	     ;; Anything else must be the script to debug.
	     (t (setq script-name arg)
		(setq script-args args))
	     )))
	(list debugger-args nil script-args annotate-p)))))

(defvar realgud:undodb-gdb-command-name)

(defun realgud:undodb-gdb-executable (file-name)
  "Return a priority for whether FILE-NAME is likely we can run undodb-gdb on."
  (let ((output (shell-command-to-string
		 (format "file %s" (file-chase-links file-name)))))
    (cond
     ((string-match "ASCII" output) 2)
     ((string-match "ELF" output) 7)
     ((string-match "executable" output) 6)
     ('t 5))))

(defun realgud:undodb-gdb-suggest-invocation (&optional debugger-name)
  "Suggest a undodb-gdb command invocation based on DEBUGGER-NAME.
Here is the priority we use:
* an executable file with the name of the current buffer stripped of
  its extension
* any executable file in the current directory with no extension
* the last invocation in undodb-gdb:minibuffer-history
* any executable in the current directory
When all else fails return the empty string."
  (let* ((file-list (directory-files default-directory))
	 (priority 2)
	 (best-filename nil)
	 (try-filename (file-name-base (or (buffer-file-name) "undodb-gdb"))))
    (when (member try-filename (directory-files default-directory))
      (setq best-filename try-filename)
      (setq priority (+ (undodb-gdbealgud:gdb-executable try-filename) 2)))

  (while (and (setq try-filename (car-safe file-list)) (< priority 8))
    (setq file-list (cdr file-list))
    (if (and (file-executable-p try-filename)
           (not (file-directory-p try-filename)))
        (if (equal try-filename (file-name-sans-extension try-filename))
            (progn
              (setq best-filename try-filename)
              (setq priority
                    (1+ (realgud:undodb-gdb-executable best-filename))))
          ;; else
          (progn
            (setq best-filename try-filename)
            (setq priority (realgud:undodb-gdb-executable best-filename))
            ))
      ))
  (if (< priority 8)
      (cond
       (realgud:undodb-gdb-minibuffer-history
        (car realgud:undodb-gdb-minibuffer-history))
       ((equal priority 7)
        (concat "undodb-gdb " best-filename))
       (t "undodb-gdb "))
    ;; else
    (concat "undodb-gdb " best-filename))
  ))

(defun realgud:undodb-gdb-reset ()
  "Undodb-Gdb cleanup.
Remove debugger's internal buffers (frame,breakpoints, etc.)."
  (interactive)
  ;; (undodb-gdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*undodb-gdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))


(defun realgud:undodb-gdb-customize ()
  "Use `customize' to edit the settings of the `realgud:undodb-gdb' debugger."
  (interactive)
  (customize-group 'realgud:undodb-gdb))

(provide-me "realgud:undodb-gdb-")
;;; core.el ends here
