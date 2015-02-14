;;; Copyright (C) 2010, 2012-2015 Rocky Bernstein <rocky@gnu.org>
;;; source-code buffer code
(eval-when-compile
  (require 'cl-lib)
  (defvar realgud-srcbuf-info) ;; is buffer local
  (defvar realgud-cmdbuf-info) ;; in the cmdbuf, this is buffer local
  )

(require 'load-relative)
(require-relative-list '("../helper" "../key") "realgud-")

(declare-function realgud-populate-common-keys 'realgud-menu)
(declare-function buffer-killed?               'realgud-helper)
(declare-function buffer-loc-line-number?      'realgud-loc)
(declare-function realgud-cmdbuf-add-srcbuf    'realgud-cmdbuf)
(declare-function realgud-cmdbuf-info-bp-list  'realgud-cmdbuf)
(declare-function realgud-loc-marker           'realgud-loc)
(declare-function realgud-loc-line-number      'realgud-loc)
(declare-function realgud-loc-num              'realgud-loc)
(declare-function make-realgud-loc-hist        'realgud-lochist)
(declare-function realgud-get-srcbuf           'helper)
(declare-function realgud-short-key-mode-setup 'realgud-shortkey)

(defstruct realgud-srcbuf-info
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

(defun realgud-srcbuf-info-set? ()
  "Return true if `realgud-srcbuf-info' is set."
  (and (boundp 'realgud-srcbuf-info)
       realgud-srcbuf-info
       (realgud-srcbuf-info? realgud-srcbuf-info)))

(defun realgud-srcbuf? (&optional buffer)
  "Return true if BUFFER is a debugger source buffer."
  (with-current-buffer-safe (or buffer (current-buffer))
    (and (realgud-srcbuf-info-set?)
	 (not (buffer-killed? (realgud-sget 'srcbuf-info 'cmdproc)))
   )))

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

(defun realgud-srcbuf-init
  (src-buffer cmdproc-buffer)
  "Initialize SRC-BUFFER as a source-code buffer for a debugger.
CMDPROC-BUFFER is the process-command buffer containing the
debugger.  DEBUGGER-NAME is the name of the debugger as a main
program name."
  (with-current-buffer cmdproc-buffer
    (set-buffer src-buffer)
    (set (make-local-variable 'realgud-srcbuf-info)
	 (make-realgud-srcbuf-info
	  :cmdproc cmdproc-buffer
	  :loc-hist (make-realgud-loc-hist)))
    (put 'realgud-srcbuf-info 'variable-documentation
	 "Debugger information for a buffer containing source code.")))

(defun realgud-srcbuf-init-or-update (src-buffer cmdproc-buffer)
  "Call `realgud-srcbuf-init' for SRC-BUFFER update `realgud-srcbuf-info' variables
in it with those from CMDPROC-BUFFER"
  (realgud-cmdbuf-add-srcbuf src-buffer cmdproc-buffer)
  (with-current-buffer-safe src-buffer
    (realgud-populate-common-keys
     ;; use-local-map returns nil so e have to call (current-local-map)
     ;; again in this case.
     (or (current-local-map) (use-local-map (make-sparse-keymap))
	 (current-local-map)))
    (if (realgud-srcbuf-info? realgud-srcbuf-info)
	(realgud-srcbuf-info-cmdproc= cmdproc-buffer)
      (realgud-srcbuf-init src-buffer cmdproc-buffer))))

(defun realgud:cmdbuf-associate(cmdbuf-name)
"Associate a command buffer with for the current buffer which is
assumed to be a source-code buffer"
  (interactive "brealgud command buffer: ")
  (realgud-srcbuf-init-or-update (current-buffer) (get-buffer cmdbuf-name))
  (realgud-short-key-mode-setup 't)
  )

(defun realgud-srcbuf-bp-list(&optional buffer)
  "Return a list of breakpoint loc structures that reside in
BUFFER which should be an initialized source buffer."
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
