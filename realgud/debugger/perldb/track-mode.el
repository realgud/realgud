;;; Copyright (C) 2011-2013 Rocky Bernstein <rocky@gnu.org>
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
(require-relative-list '("core" "init") "realgud-perldb-")

(realgud-track-mode-vars "realgud-perldb")

(declare-function realgud-perl-populate-command-keys
		  'realgud-perldb)
(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud-track-set-debugger 'realgud-track-mode)

(realgud-perl-populate-command-keys realgud-perldb-track-mode-map )

;; Perldb doesn't have stack switching commands.
(define-key realgud-perldb-short-key-mode-map
  [remap dbg-cmd-newer-frame] 'undefined)
(define-key realgud-perldb-short-key-mode-map
  [remap realgud-cmd-older-frame] 'undefined)

(defun realgud-perldb-track-mode-hook()
  (if realgud-perldb-track-mode
      (progn
	(use-local-map realgud-perldb-track-mode-map)
	(message "using perldb mode map")
	)
    (message "perldb track-mode-hook disable called"))
)

(define-minor-mode realgud-perldb-track-mode
  "Minor mode for tracking Perl debugging inside a process shell."
  :init-value nil
  ;; :lighter " perldb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'perldb
  :keymap realgud-perldb-track-mode-map

  (realgud-track-set-debugger "perldb")
  (if realgud-perldb-track-mode
      (progn
	(setq realgud-track-mode 't)
	(run-mode-hooks (intern (realgud-perldb-track-mode-hook))))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud-perldb-")
