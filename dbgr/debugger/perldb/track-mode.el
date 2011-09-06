;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Stock Perl Debugger "perldb5" tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds" 
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 ) 
		       "dbgr-")
(require-relative-list '("core" "init") "dbgr-perldb-")

(dbgr-track-mode-vars "dbgr-perldb")

(declare-function dbgr-track-mode(bool))

(define-key dbgr-perldb-track-mode-map 
  (kbd "C-c !!") 'dbgr-goto-lang-backtrace-line)
(define-key dbgr-perldb-track-mode-map 
  (kbd "C-c !b") 'dbgr-goto-debugger-backtrace-line)

;; Perldb doesn't have stack switching commands.
(define-key dbgr-perldb-short-key-mode-map
  [remap dbg-cmd-newer-frame] 'undefined)
(define-key dbgr-perldb-short-key-mode-map
  [remap dbgr-cmd-older-frame] 'undefined)

(defun dbgr-perldb-track-mode-hook()
  (if dbgr-perldb-track-mode
      (progn
	(use-local-map dbgr-perldb-track-mode-map)
	(message "using perldb mode map")
	)
    (message "perldb track-mode-hook disable called"))
)

(define-minor-mode dbgr-perldb-track-mode
  "Minor mode for tracking Perl debugging inside a process shell."
  :init-value nil
  ;; :lighter " perldb"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'perldb
  :keymap dbgr-perldb-track-mode-map

  (dbgr-track-set-debugger "perldb")
  (if dbgr-perldb-track-mode
      (progn 
	(dbgr-track-mode 't)
	(run-mode-hooks (intern (dbgr-perldb-track-mode-hook))))
    (progn 
      (dbgr-track-mode nil)
      ))
)

(provide-me "dbgr-perldb-")
