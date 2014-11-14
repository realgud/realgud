;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

;; We use gud to handle the classpath-to-filename mapping
(require 'gud)

(require 'load-relative)
(require-relative-list '("../../common/track"
                         "../../common/core"
                         "../../common/lang"
                         "../../common/file")
                       "realgud-")
(require-relative-list '("init") "realgud:jdb-")

(declare-function gud-find-source            'gud)

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud-file-loc-from-line 'realgud-file)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:jdb-minibuffer-history nil
  "minibuffer history list for the command `realgud:jdb'.")

(easy-mmode-defmap jdb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud:jdb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'jdb-suggest-invocation
   jdb-minibuffer-local-map
   'realgud:jdb-minibuffer-history
   opt-debugger))

(defun realgud:jdb-loc-fn-callback(text filename lineno source-str
					ignore-file-re cmd-mark)

  (realgud-file-loc-from-line (or (gud-jdb-find-source filename)
				  (concat filename ".java"))
			      lineno cmd-mark
			      source-str nil
			      ignore-file-re))

(defun realgud:jdb-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing

* the command debuger (e.g. jdb)

* debugger command rguments if any - a list of strings

* the script name and its arguments - list of strings

For example for the following input
   '(\"jdb\" \"-classpath . ./TestMe.java a b\"))

we might return:
   (\"jdb\" nil \"TestMe\"))

Note that the script name path has been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  [ruby ruby-options] jdb jdb-options script-name script-options
  (let (
        (args orig-args)
	(interp-regexp
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^jdb*\\(.exe\\)?$"
	   "^jdb*$"))
	(jdb-name)
        ;;
        ;; One dash is added automatically to the below, so
        ;; attach is really -attach
	(jdb-two-args '("attach" "sourcepath" "classpath" "dbgtrace"))

        ;; Things returned
        (debugger-args '())
        (program-args '()))

    (if (not (and args))
        ;; Got nothing: return '(nil nil nil)
        (list jdb-name nil debugger-args program-args)
      ;; else
      ;; Strip off optional "jdb" or "jdb.exe" etc.
      (when (string-match interp-regexp (car args))
	(setq jdb-name (car args))
        (setq program-args (nconc program-args (cdr args))))

      (list jdb-name debugger-args program-args))))

;; To silence Warning: reference to free variable
(defvar realgud:jdb-command-name)

(defun jdb-suggest-invocation (debugger-name)
  "Suggest a jdb command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation realgud:jdb-command-name
			      realgud:jdb-minibuffer-history
			      "java" "\\.java$" "jdb"))

(defun jdb-reset ()
  "Jdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (jdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*jdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun jdb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'jdb-debugger-support-minor-mode minor-mode-map-alist)
;;        jdb-debugger-support-minor-mode-map-when-deactive))


(defun realgud:jdb-customize ()
  "Use `customize' to edit the settings of the `jdb' debugger."
  (interactive)
  (customize-group 'realgud:jdb))

(provide-me "realgud:jdb-")
