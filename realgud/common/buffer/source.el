;;; Copyright (C) 2010, 2012-2015, 2017, 2019 Free Software Foundation, Inc

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

;;; source-code buffer code
(eval-when-compile
  (defvar realgud-srcbuf-info) ;; is buffer local
  (defvar realgud-cmdbuf-info) ;; in the cmdbuf, this is buffer local
  )

(require 'cl-lib)
(require 'load-relative)
(require-relative-list '("../helper" "../key") "realgud-")

(declare-function realgud-populate-common-keys 'realgud-menu)
(declare-function buffer-killed?               'realgud-helper)
(declare-function buffer-loc-line-number?      'realgud-loc)
(declare-function realgud-cmdbuf-add-srcbuf    'realgud-cmdbuf)
(declare-function realgud-cmdbuf-info-bp-list  'realgud-cmdbuf)
(declare-function realgud-cmdbuf?              'realgud-cmdbuf)
(declare-function realgud-loc-marker           'realgud-loc)
(declare-function realgud-loc-line-number      'realgud-loc)
(declare-function realgud-loc-num              'realgud-loc)
(declare-function make-realgud-loc-hist        'realgud-lochist)
(declare-function realgud-get-srcbuf           'helper)
(declare-function realgud-short-key-mode-setup 'realgud-shortkey)

(cl-defstruct realgud-srcbuf-info
  "debugger object/structure specific to a (top-level) source program
to be debugged."
  cmdproc        ;; buffer of the associated debugger process
  cur-pos        ;; If not nil, the debugger thinks we are currently
		 ;; positioned at a corresponding place in the
		 ;; program.
  short-key?     ;; Was the source buffer previously in short-key
		 ;; mode? Used to deterimine when short-key mode
		 ;; changes state in a source buffer, so we need to
		 ;; perform on/off actions.
  was-read-only? ;; Was buffer initially read only? (i.e. the original
		 ;; value of the buffer's buffer-read-only
		 ;; variable. Short-key-mode may change the read-only
		 ;; state, so we need restore this value when leaving
		 ;; short-key mode
  prev-local-map ;; Local map before enabling short-key-mode

  loc-hist       ;; ring of locations seen

  ;; FILL IN THE FUTURE
  ;;(brkpt-alist '())  ;; alist of breakpoints the debugger has referring
                       ;; to this buffer. Each item is (brkpt-name . marker)
  ;;
)


(defalias 'realgud-srcbuf-info? 'realgud-srcbuf-p)

;; FIXME: figure out how to put in a loop.
(realgud-struct-field-setter "realgud-srcbuf-info" "cmdproc")
(realgud-struct-field-setter "realgud-srcbuf-info" "short-key?")
(realgud-struct-field-setter "realgud-srcbuf-info" "was-read-only?")
(realgud-struct-field-setter "realgud-srcbuf-info" "prev-local-map")

(defun realgud-srcbuf-info-set? ()
  "Return non-nil if `realgud-srcbuf-info' is set."
  (and (bound-and-true-p realgud-srcbuf-info)
       (realgud-srcbuf-info? realgud-srcbuf-info)))

(defun realgud-srcbuf? (&optional buffer)
  "Return non-nil if BUFFER is a debugger source buffer."
  (with-current-buffer-safe (or buffer (current-buffer))
    (and (realgud-srcbuf-info-set?)
	 (not (buffer-killed? (realgud-sget 'srcbuf-info 'cmdproc)))
   )))

(defun realgud--read-cmd-buf (prompt)
  "Read a command buffer, prompting with PROMPT."
  (let* ((cmd-bufs (cl-remove-if-not #'realgud-cmdbuf? (buffer-list)))
         (buf-names (mapcar #'buffer-name cmd-bufs))
         (default (car buf-names)))
    (when buf-names
      ;; Use completing-read instead of read-buffer: annoyingly, ido's
      ;; read-buffer ignores predicates.
      (setq prompt (format "%s (default: %s): " prompt default))
      (get-buffer (completing-read prompt buf-names nil t nil nil default)))))

(defun realgud--ensure-attached (&optional src-buf)
  "Try to attach SRC-BUF to a command buffer.
If SRC-BUF is already attached, do nothing.  Otherwise, prompt
the user for a command buffer to associate SRC-BUF to.  Returns
non-nil if association was successful.  SRC-BUF defaults to
current buffer."
  (setq src-buf (or src-buf (current-buffer)))
  (unless (realgud-srcbuf? src-buf)
    (let ((cmd-buf (realgud--read-cmd-buf "Command buffer to attach to")))
      (if cmd-buf
          (realgud-srcbuf-init src-buf cmd-buf)
        (message "No debugger process found to attach %s to" (buffer-name)))))
  (realgud-srcbuf? src-buf))

(defun realgud-srcbuf-debugger-name (&optional src-buf)
  "Return the debugger name recorded in the debugger command-process buffer."
  (with-current-buffer-safe (or src-buf (current-buffer))
    (realgud-sget 'srcbuf-info 'debugger-name))
)

(defun realgud-srcbuf-loc-hist(src-buf)
  "Return the history ring of locations that a debugger process has stored."
  (with-current-buffer-safe src-buf
    (realgud-sget 'srcbuf-info 'loc-hist))
)

(declare-function fn-p-to-fn?-alias(sym))
(fn-p-to-fn?-alias 'realgud-srcbuf-info-p)
(declare-function realgud-srcbuf-info?(var))
(declare-function realgud-cmdbuf-info-name(cmdbuf-info))

;; FIXME: support a list of cmdprocs's since we want to allow
;; a source buffer to potentially participate in several debuggers
;; which might be active.
(make-variable-buffer-local 'realgud-srcbuf-info)

(defvar realgud:srcbuf-mode-map
  (make-sparse-keymap) )

(define-minor-mode realgud-srcbuf-mode
  "Minor mode for source buffers for the `realgud' debugger."
  :group 'realgud
  :global nil
  :init-value nil
  :keymap realgud:srcbuf-mode-map
)

(defun realgud-srcbuf-init
  (src-buffer cmdproc-buffer)
  "Initialize SRC-BUFFER as a source-code buffer for a debugger.
CMDPROC-BUFFER is the process-command buffer containing the
debugger."
  (with-current-buffer-safe cmdproc-buffer
    (when (bufferp src-buffer)
      (set-buffer src-buffer)
      (realgud-cmdbuf-add-srcbuf src-buffer cmdproc-buffer)
      (set (make-local-variable 'realgud-srcbuf-info)
	   (make-realgud-srcbuf-info
	    :cmdproc cmdproc-buffer
	    :loc-hist (make-realgud-loc-hist)))
      (put 'realgud-srcbuf-info 'variable-documentation
	   "Debugger information for a buffer containing source code."))))

(defun realgud-srcbuf-init-or-update (src-buffer cmdproc-buffer)
  "Call `realgud-srcbuf-init' for SRC-BUFFER update `realgud-srcbuf-info' variables
in it with those from CMDPROC-BUFFER"
  (realgud-cmdbuf-add-srcbuf src-buffer cmdproc-buffer)
  (with-current-buffer-safe src-buffer
    (realgud-srcbuf-mode)
    (realgud-populate-common-keys realgud:srcbuf-mode-map)
    (if (realgud-srcbuf-info? realgud-srcbuf-info)
	(realgud-srcbuf-info-cmdproc= cmdproc-buffer)
      (realgud-srcbuf-init src-buffer cmdproc-buffer))))

(defun realgud:cmdbuf-associate ()
  "Associate a command buffer with the current (source-code) buffer."
  ;; realgud-short-key-mode-setup will attempt to associate if needed.
  (realgud-short-key-mode-setup t))

(defun realgud-srcbuf-bp-list(&optional buffer)
  "Return a list of breakpoint loc structures that reside in
BUFFER. BUFFER should be an initialized source buffer."
  (let ((src-buffer (realgud-get-srcbuf buffer)))
    (if src-buffer
	(with-current-buffer src-buffer
	(let* ((info realgud-srcbuf-info)
	       (cmdbuf (realgud-srcbuf-info-cmdproc info)))
	  (with-current-buffer cmdbuf
	    (let ((bp-list
		   (realgud-cmdbuf-info-bp-list realgud-cmdbuf-info)))
	      (delq nil
		    (mapcar (lambda (loc)
			      (cond ((eq src-buffer
					 (marker-buffer (realgud-loc-marker loc)))
				     loc)
				    (nil)))
			    bp-list))
	      )))))))

(defun realgud-get-bpnum-from-line-num(line-num &optional buffer)
  "Find a breakpoint number associated with LINE-NUM in source code BUFFER.
If none exists return nil"
  (let ((src-buffer (realgud-get-srcbuf buffer))
	(bp-num nil)
	(bp)
	(bp-list)
	)
    (if src-buffer
	(progn
	  (setq bp-list (realgud-srcbuf-bp-list src-buffer))
	  (while (and (not bp-num) bp-list)
	    (setq bp (car bp-list))
	    (setq bp-list (cdr bp-list))
	    (if (eq line-num (realgud-loc-line-number bp))
		(setq bp-num (realgud-loc-num bp)))
	    ))
      )
    bp-num))

(provide-me "realgud-buffer-")
