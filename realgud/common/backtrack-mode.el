;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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

;;  Used to parse programming-language backtrace-like tracks
;;  output. In contrast to track-mode, there doesn't have to be a
;;  process shell arround Compare with backtrace-mode.el which
;;  handles backtraces inside the debugger

(require 'shell)

(require 'load-relative)
(require-relative-list
 '("core"   "helper" "track" "loc" "lochist" "file"
   "fringe" "window" "regexp" "menu"
   "send"   "shortkey") "realgud-")

(declare-function realgud:debugger-name-transform 'realgud-helper)
(declare-function realgud-populate-debugger-menu  'realgud-menu)
(declare-function realgud:track-set-debugger      'realgud-track)

(defvar realgud-backtrack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [frames-menu]
      (list 'menu-item "Specific Frames" 'realgud:frames-menu))
    (define-key map [M-right]	'realgud-track-hist-newest)
    (define-key map [M-down]	'realgud-track-hist-newer)
    (define-key map [M-up]	'realgud-track-hist-older)
    (define-key map [M-print]	'realgud-track-hist-older)
    (define-key map [M-S-down]	'realgud-track-hist-newest)
    (define-key map [M-S-up]	'realgud-track-hist-oldest)
    (realgud-populate-debugger-menu map)
    map)
  "Keymap used in `realgud-backtrack-minor-mode'.")

;; FIXME figure out if I can put this in something like a header file.
;; FIXME: combine with realgud:track-set-debugger's completing read
(defun realgud-backtrack-set-debugger (debugger-name)
  "Set debugger name This info is returned or nil if we can't find a
debugger with that information"
  (interactive
   (list (completing-read "Debugger name: " realgud-pat-hash)))
  (let ((regexp-hash (gethash debugger-name realgud-pat-hash)))
    (if regexp-hash
	(let* ((base-variable-name
		(or (gethash debugger-name realgud:variable-basename-hash)
		    debugger-name))
	       (specific-track-mode (intern (concat base-variable-name "-backtrack-mode")))
	       )
	  (if (and (not (eval specific-track-mode))
		   (functionp specific-track-mode))
	      (funcall specific-track-mode 't))
	  )
      (progn
	(message "I Don't have %s listed as a debugger." debugger-name)
	nil)
      )))

(define-minor-mode realgud-backtrack-mode
  "Minor mode for backtracking parsing."
  :init-value nil
  :global nil
  :group 'realgud

  :lighter
  (:eval (progn
	   (concat " "
		   (if (realgud-cmdbuf-info-set?)
		       (realgud-sget 'cmdbuf-info 'debugger-name)
		     "dbgr??"))))

  :keymap realgud-backtrack-mode-map
  ;; Setup/teardown
  )

(defmacro realgud-backtrack-mode-vars (name)
  `(progn
     (defvar ,(intern (concat name "-backtrack-mode")) nil
	,(format "Non-nil if using %s-backtrack-mode as a minor mode of some other mode.
Use the command `%s-track-mode' to toggle or set this variable." name name))
     (defvar ,(intern (concat name "-backtrack-mode-map")) (make-sparse-keymap)
       ,(format "Keymap used in `%s-backtrack-mode'." name))
    ))

;; FIXME: The below could be a macro? I have a hard time getting
;; macros right.
(defun realgud-backtrack-mode-body(name)
  "Used in by custom debuggers: pydbgr, trepan, gdb, etc. NAME is
the name of the debugger which is used to preface variables."
  (realgud:track-set-debugger name)
  (funcall (intern (concat "realgud-define-" name "-commands")))
  (if (intern (concat name "-backtrack-mode"))
      (progn
	(realgud-backtrack-mode 't)
	(run-mode-hooks (intern (concat name "-backtrack-mode-hook"))))
    (progn
      (realgud-backtrack-mode nil)
      )))

(provide-me "realgud-")
