;;; Copyright (C) 2011-2012, 2014 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/track"
                         "../../common/core"
                         "../../common/lang")
                       "realgud-")
(require-relative-list '("init") "realgud-trepanpl-")

(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar trepanpl-minibuffer-history nil
  "minibuffer history list for the command `trepanpl'.")

(easy-mmode-defmap realgud-trepanpl-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of trepanpl startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud-trepanpl-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud-trepanpl-suggest-invocation
   realgud-trepanpl-minibuffer-local-map
   'realgud-trepanpl-minibuffer-history
   opt-debugger))

(defun realgud-trepanpl-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing

- the command processor (e.g. perl) and it's arguments if any - a
  list of strings

- the script name and its arguments - list of strings

For example for the following input
  (map 'list 'symbol-name
   '(perl5.10 -W -C /tmp trepan.pl --emacs ./gcd.pl a b))

we might return:
   ((perl -W -C) (trepan.pl --emacs) (./gcd.pl a b) 't)

NOTE: the above should have each item listed in quotes.
"

  ;; Parse the following kind of pattern:
  ;;  [perl perl-options] trepanpl trepanpl-options script-name script-options
  (let (
        (args orig-args)
        (pair)          ;; temp return from
        (perl-opt-two-args '("0" "C" "D" "i" "l" "m" "-module" "x"))
        ;; Perl doesn't have mandatory 2-arg options in our sense,
        ;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
        ;;
        (perl-two-args '())
        ;; One dash is added automatically to the below, so
        ;; h is really -h and -host is really --host.
        (trepanpl-two-args '("h" "-host" "p" "-port"
                           "I" "-include"))
        (trepanpl-opt-two-args '())
        (interp-regexp
         (if (member system-type (list 'windows-nt 'cygwin 'msdos))
             "^perl\\(?:5[0-9.]*\\)\\(.exe\\)?$"
           "^perl\\(?:5[0-9.]*\\)?$"))

        ;; Things returned
        (script-name nil)
        (debugger-name nil)
        (interpreter-args '())
        (debugger-args '())
        (script-args '())
        (annotate-p nil))

    (if (not (and args))
        ;; Got nothing: return '(nil, nil)
        (list interpreter-args debugger-args script-args annotate-p)
      ;; else
      ;; Strip off optional "perl" or "perl5.10.1" etc.
      (when (string-match interp-regexp
                          (file-name-sans-extension
                           (file-name-nondirectory (car args))))
        (setq interpreter-args (list (pop args)))

        ;; Strip off optional "perl" or "perl5.10.1" etc.
        (while (and args
                    (string-match "^-" (car args)))
          (setq pair (realgud-parse-command-arg
                      args perl-two-args perl-opt-two-args))
          (nconc interpreter-args (car pair))
          (setq args (cadr pair))))

      (list interpreter-args args))
    ))

; # To silence Warning: reference to free variable
(defvar realgud-trepanpl-command-name)

(defun realgud-trepanpl-suggest-invocation (debugger-name)
  "Suggest a trepanpl command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation realgud-trepanpl-command-name
                           trepanpl-minibuffer-history
                           "perl" "\\.pl$" "trepan.pl"))

(defun realgud-trepanpl-reset ()
  "Trepanpl cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (trepanpl-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*trepanpl-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun trepanpl-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'trepanpl-debugger-support-minor-mode minor-mode-map-alist)
;;        trepanpl-debugger-support-minor-mode-map-when-deactive))


(defun realgud-trepanpl-customize ()
  "Use `customize' to edit the settings of the `trepanpl' debugger."
  (interactive)
  (customize-group 'realgud-trepanpl))

(provide-me "realgud-trepanpl-")
