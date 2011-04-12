;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org> Used to parse
;;;  programming-language backtrace-like tracks output. In contrast to
;;;  track-mode, there doesn't have to be a process shell arround
;;; Compare with backtrace-mode.el which handles backtraces inside the
;;; debugger

(eval-when-compile (require 'cl))
(require 'shell)

(require 'load-relative)
(require-relative-list
 '("core"   "helper" "track" "loc" "lochist" "file" 
   "fringe" "window" "regexp" "menu"
   "send"   "shortkey") "dbgr-")

(require-relative-list  '("buffer/command") "dbgr-buffer-")

(defvar dbgr-backtrack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right]	'dbgr-track-hist-newest)
    (define-key map [M-down]	'dbgr-track-hist-newer)
    (define-key map [M-up]	'dbgr-track-hist-older)
    (define-key map [M-print]	'dbgr-track-hist-older)
    (define-key map [M-S-down]	'dbgr-track-hist-newest)
    (define-key map [M-S-up]	'dbgr-track-hist-oldest)
    (dbgr-populate-debugger-menu map)
    map)
  "Keymap used in `dbgr-backtrack-minor-mode'.")

;; FIXME figure out if I can put this in something like a header file.
(defun dbgr-backtrack-set-debugger (debugger-name)
  "Set debugger name This info is returned or nil if we can't find a 
debugger with that information"
  (interactive "sDebugger name: ")
  (let ((regexp-hash (gethash debugger-name dbgr-pat-hash)))
    (if regexp-hash
	(let* ((prefix 
		(if (equal debugger-name "gdb") "dbgr-gdb" debugger-name))
	       (specific-track-mode (intern (concat prefix "-backtrack-mode")))
	       )
	  (if (and (not (eval specific-track-mode))
		   (functionp specific-track-mode))
	      (funcall specific-track-mode 't))
	  )
      (progn 
	(message "I Don't have %s listed as a debugger." debugger-name)
	nil)
      )))

(define-minor-mode dbgr-backtrack-mode
  "Minor mode for backtracking parsing."
  :init-value nil
  :global nil
  :group 'dbgr

  :lighter 
  (:eval (progn 
	   (concat " "
		   (if (dbgr-cmdbuf-info-set?)
		       (dbgr-sget 'cmdbuf-info 'debugger-name)
		     "dbgr??"))))

  :keymap dbgr-backtrack-mode-map
  ;; Setup/teardown
  )

(defmacro dbgr-backtrack-mode-vars (name)
  `(progn
     (defvar ,(intern (concat name "-backtrack-mode")) nil
	,(format "Non-nil if using %s-backtrack-mode as a minor mode of some other mode.
Use the command `%s-track-mode' to toggle or set this variable." name name))
     (defvar ,(intern (concat name "-backtrack-mode-map")) (make-sparse-keymap)
       ,(format "Keymap used in `%s-backtrack-mode'." name))
    ))

;; FIXME: The below could be a macro? I have a hard time getting
;; macros right.
(defun dbgr-backtrack-mode-body(name)
  "Used in by custom debuggers: pydbgr, trepan, gdb, etc. NAME is
the name of the debugger which is used to preface variables."
  (dbgr-track-set-debugger name)
  (funcall (intern (concat "dbgr-define-" name "-commands")))
  (if (intern (concat name "-backtrack-mode"))
      (progn 
	(dbgr-backtrack-mode 't)
	(run-mode-hooks (intern (concat name "-backtrack-mode-hook"))))
    (progn 
      (dbgr-backtrack-mode nil)
      )))

(provide-me "dbgr-")
