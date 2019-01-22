;; Copyright (C) 2010-2015, 2017 Free Software Foundation, Inc

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

(require 'load-relative)
(require-relative-list '("cmds" "custom" "eval" "helper" "key" "lochist" "loc"
			 "menu")
		       "realgud-")
(require-relative-list '("buffer/command" "buffer/helper" "buffer/source")
		       "realgud-buffer-")

(eval-when-compile
  (defvar realgud:tool-bar-map) ;; Fully defined in track-mode
)


(declare-function realgud-cmds--mouse-add-remove-bp     'realgud-cmds)
(declare-function realgud-cmdbuf?                       'realgud-buffer-command)
(declare-function realgud:debugger-name-transform       'realgud-helper)
(declare-function realgud-get-cmdbuf                    'realgud-buffer-helper)
(declare-function realgud:follow-mark                   'realgud-follow)
(declare-function realgud-loc-hist-item-at              'realgud-lochist)
(declare-function realgud-cmdbuf-loc-hist               'realgud-command)
(declare-function realgud-populate-debugger-menu        'realgud-menu)
(declare-function realgud-populate-common-keys          'realgud-key)
(declare-function realgud-populate-src-buffer-map-plain 'realgud-key)
(declare-function realgud-srcbuf-info-short-key?=,      'realgud-source)
(declare-function realgud-srcbuf-info-was-read-only?=   'realgud-source)
(declare-function realgud-srcbuf-info-prev-local-map=   'realgud-source)
(declare-function realgud-srcbuf?                       'realgud-buffer-source)
(declare-function realgud--ensure-attached              'realgud-buffer-source)
(declare-function realgud-srcbuf-info-set?              'realgud-buffer-source)

;; (defvar realgud::tool-bar-map) ;; fully defined in track-mode.el

(defvar realgud:shortkey-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (realgud-populate-debugger-menu map)
    (realgud-populate-common-keys map)
    (realgud-populate-src-buffer-map-plain map)
    (define-key map "1"        'realgud-goto-arrow1)
    (define-key map "2"        'realgud-goto-arrow2)
    (define-key map "3"        'realgud-goto-arrow3)
    (define-key map "4"        'realgud:goto-loc-hist-4)
    (define-key map "5"        'realgud:goto-loc-hist-5)
    (define-key map "6"        'realgud:goto-loc-hist-6)
    (define-key map "7"        'realgud:goto-loc-hist-7)
    (define-key map "8"        'realgud:goto-loc-hist-8)
    (define-key map "9"        'realgud:goto-loc-hist-9)
    (define-key map "b"        'realgud:cmd-break)
    (define-key map "j"        'realgud:cmd-jump)
    (define-key map "c"        'realgud:cmd-continue)
    (define-key map "e"        'realgud:cmd-eval-dwim)
    (define-key map "E"        'realgud:cmd-eval-at-point)
    (define-key map "U"        'realgud:cmd-until)
    (define-key map "h"        'realgud:cmd-until-here)
    
    (define-key map [mouse-2]  'realgud:tooltip-eval)
    (define-key map [left-fringe mouse-1] #'realgud-cmds--mouse-add-remove-bp)
    (define-key map [left-margin mouse-1] #'realgud-cmds--mouse-add-remove-bp)

    ;; FIXME: these can go to a common routine
    (define-key map "<"        'realgud:cmd-newer-frame)
    (define-key map ">"        'realgud:cmd-older-frame)
    (define-key map "d"        'realgud:cmd-newer-frame)
    (define-key map "u"        'realgud:cmd-older-frame)
    (define-key map "l"        'realgud-recenter-arrow)
    (define-key map "C"        'realgud-window-cmd-undisturb-src)
    (define-key map "I"        'realgud:cmdbuf-info-describe)
    (define-key map "S"        'realgud-window-src-undisturb-cmd)

    (define-key map "R"        'realgud:cmd-restart)
    (define-key map "!"        'realgud:cmd-shell)
    (define-key map [insert]   'realgud-short-key-mode)
    (define-key map [(control x)(control q)] 'realgud-short-key-mode)
    map)
  "Keymap used in `realgud-short-key-mode'.")

;; Implementation note: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode realgud-short-key-mode
  "Minor mode with short keys for source buffers for the `realgud' debugger.
If `realgud-srcbuf-lock' is set, the buffer is read-only when the
minor mode is active.

\\{realgud:shortkey-mode-map}"
  :group 'realgud
  :global nil
  :init-value nil
  :lighter " ShortKeys"
  :keymap realgud:shortkey-mode-map
  ;; executed on activation/deactivation:
  (realgud-short-key-mode-setup realgud-short-key-mode))

(defun realgud-get-short-key-mode-map (cmdbuf)
  (when (realgud-cmdbuf? cmdbuf)
    (with-current-buffer cmdbuf
      (let* ((info realgud-cmdbuf-info)
	     (debugger-name (realgud-cmdbuf-info-debugger-name info))
	     (base-variable-name
	      (or (gethash debugger-name realgud:variable-basename-hash)
	      debugger-name))
	     (keymap-symbol
	      (intern
	       (replace-regexp-in-string
		"\\." ""
		(concat base-variable-name "-short-key-mode-map"))))
	     (keymap (eval keymap-symbol))
	     )
	(cond ((keymapp keymap) keymap)
	      ('t nil))
	))
    ))

(defun realgud-short-key-mode-setup (mode-on?)
  "Set up or tear down `realgud-short-key-mode'.
MODE-ON? is a boolean indicating whether the mode should be
turned on or off."
  (setq realgud-short-key-mode mode-on?)
  ;; When enabling, try to find a command buffer to attach to.
  (when (and realgud-short-key-mode (not (realgud--ensure-attached)))
    (setq realgud-short-key-mode nil))
  ;; Now apply mode change
  (cond
   ;; Mode was just enabled
   (realgud-short-key-mode
    ;; Record info to restore it when disabling
    (unless (equal (realgud-sget 'srcbuf-info 'short-key?) realgud-short-key-mode)
      (realgud-srcbuf-info-prev-local-map= (current-local-map))
      (realgud-srcbuf-info-was-read-only?= buffer-read-only))
    ;; Apply local map
    (let ((keymap (realgud-get-short-key-mode-map (realgud-get-cmdbuf))))
      (when keymap (use-local-map keymap)))
    ;; Finish setting up
    (set (make-local-variable 'tool-bar-map) realgud:tool-bar-map)
    (local-set-key [m-insert] #'realgud-short-key-mode)
    (setq buffer-read-only realgud-srcbuf-lock)
    (run-mode-hooks 'realgud-short-key-mode-hook))
   ;; Mode was just disabled
   (t
    (kill-local-variable 'tool-bar-map)
    (when (realgud-srcbuf-info-set?)
      ;; Restore previous state
      (use-local-map (realgud-sget 'srcbuf-info 'prev-local-map))
      (setq buffer-read-only (realgud-sget 'srcbuf-info 'was-read-only?)))))
  ;; Record state
  (when (realgud-srcbuf-info-set?)
    (realgud-srcbuf-info-short-key?= realgud-short-key-mode)))

(defun realgud-short-key-mode-off ()
  "Turn off `realgud-short-key-mode' in all buffers."
  (interactive)
  (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when realgud-short-key-mode
	  (realgud-short-key-mode -1)))))

(defun realgud-populate-src-buffer-map (map)
  "Bind all common keys and menu used in src buffers.
This includes the keys bound to `realgud-key-prefix' (typically C-x
C-a)."
  (realgud-populate-src-buffer-map-plain map)
  (realgud-populate-common-keys map)
  (let ((prefix-map (make-sparse-keymap)))
    (realgud-populate-debugger-menu map)
    (realgud-populate-src-buffer-map-plain prefix-map)
    (define-key map realgud-key-prefix prefix-map)))

(defun realgud:goto-loc-hist(num)
  "Go to position nth from the newest position."
  (let ((cmdbuf (realgud-get-cmdbuf)))
    (if cmdbuf
      (let* ((loc-hist (realgud-cmdbuf-loc-hist cmdbuf))
	    (loc (realgud-loc-hist-item-at loc-hist (- num)))
	    (loc-marker (realgud-loc-marker loc)))
	(realgud:follow-mark loc-marker))
      ;; else
	(message "No command buffer associated with this buffer")
    )))


(defun realgud:goto-loc-hist-4 ()
  "Go to position 4th from the newest position."
  (interactive)
  (realgud:goto-loc-hist 4))

(defun realgud:goto-loc-hist-5 ()
  "Go to position 5th from the newest position."
  (interactive)
  (realgud:goto-loc-hist 5))

(defun realgud:goto-loc-hist-6 ()
  "Go to position 6th from the newest position."
  (interactive)
  (realgud:goto-loc-hist 6))

(defun realgud:goto-loc-hist-7 ()
  "Go to position 7th from the newest position."
  (interactive)
  (realgud:goto-loc-hist 7))

(defun realgud:goto-loc-hist-8 ()
  "Go to position 8th from the newest position."
  (interactive)
  (realgud:goto-loc-hist 8))

(defun realgud:goto-loc-hist-9 ()
  "Go to position 9th from the newest position."
  (interactive)
  (realgud:goto-loc-hist 9))

(provide-me "realgud-")
