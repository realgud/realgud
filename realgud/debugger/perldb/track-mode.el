;;; Copyright (C) 2011-2014 Rocky Bernstein <rocky@gnu.org>
;;; Stock Perl Debugger "perldb5" tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:perldb-")
(require-relative-list '("../../lang/perl") "realgud-lang-")

(realgud-track-mode-vars "perldb")

(declare-function realgud-cmd-remap           'realgud-cmds)
(declare-function realgud-perl-populate-command-keys
		  'realgud:perldb)
(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup    realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(realgud-perl-populate-command-keys perldb-track-mode-map )

(defun perldb-track-mode-hook()
  (if perldb-track-mode
      (progn
	(use-local-map perldb-track-mode-map)
	(message "using perldb mode map")
	)
    (message "perldb track-mode-hook disable called"))
)

(define-minor-mode perldb-track-mode
  "Minor mode for tracking perl5db source locations inside a process shell via realgud. perl5db is the stock Perl debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{perldb-track-mode-map}
"
  :init-value nil
  ;; :lighter " perldb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:perldb
  :keymap perldb-track-mode-map

  (realgud:track-set-debugger "perldb")
  (if perldb-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(run-mode-hooks (intern (perldb-track-mode-hook))))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(defun realgud:perldb-backtrace(arg)
  (interactive "p")
  (realgud-cmd-remap arg "backtrace" "T" "T")
)

;; Perldb doesn't have stack switching commands.
(define-key perldb-short-key-mode-map
  [remap realgud-cmd-newer-frame] 'undefined)
(define-key perldb-short-key-mode-map
  [remap realgud-cmd-older-frame] 'undefined)
(define-key perldb-short-key-mode-map "T" 'realgud:perldb-backtrace)

(provide-me "realgud:perldb-")
