;;; source-code buffer code
(eval-when-compile 
  (require 'cl)
  (defvar dbgr-srcbuf-info) ;; is buffer local
  (defvar dbgr-cmdbuf-info) ;; in the cmdbuf, this is buffer local
  )

(require 'load-relative)
(require-relative-list '("helper" "key") "dbgr-")

(defstruct dbgr-srcbuf-info
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  debugger-name  ;; Name of debugger. We could get this from the
                 ;; process command buffer, but we want to store it
                 ;; here in case the command buffer disappears. Used
                 ;; in recomputing a suitiable debugger invocation.
  cmd-args       ;; Debugger command invocation as a list of strings
                 ;; or nil. See above about why we don't get from the
                 ;; process command buffer. Used to suggest a debugger
                 ;; invocation.
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
(defalias 'dbgr-srcbuf-info? 'dbgr-srcbuf-p)

;; FIXME: DRY = access via a macro. See also analogous
;; code in dbgr-srcbuf
(defun dbgr-srcbuf-info-was-read-only?=(info value)
  (setf (dbgr-srcbuf-info-was-read-only? info) value))

(defun dbgr-srcbuf-info-short-key?=(info value)
  (setf (dbgr-srcbuf-info-short-key? info) value))

(defun dbgr-srcbuf-info-set? ()
  "Return true if `dbgr-srcbuf-info' is set."
  (and (boundp 'dbgr-srcbuf-info) 
       dbgr-srcbuf-info
       (dbgr-srcbuf-info? dbgr-srcbuf-info)))

(defun dbgr-srcbuf? ( &optional buffer)
  "Return true if BUFFER is a debugger source buffer."
  (with-current-buffer-safe (or buffer (current-buffer))
    (and (dbgr-srcbuf-info-set?)
	 (not (buffer-killed? (dbgr-sget 'srcbuf-info 'cmdproc)))
   )))

(defun dbgr-srcbuf-debugger-name (&optional src-buf)
  "Return the debugger name recorded in the debugger command-process buffer."
  (with-current-buffer-safe (or src-buf (current-buffer))
    (dbgr-sget 'srcbuf-info 'debugger-name))
)

(defun dbgr-srcbuf-loc-hist(src-buf)
  "Return the history ring of locations that a debugger process has stored."
  (with-current-buffer-safe src-buf 
    (dbgr-sget 'srcbuf-info 'loc-hist))
)
;; FIXME: DRY = access via a macro
(defun dbgr-srcbuf-info-cmdproc=(info buffer)
  (setf (dbgr-srcbuf-info-cmdproc info) buffer))

(defun dbgr-srcbuf-info-debugger-name=(info value)
  (setf (dbgr-srcbuf-info-debugger-name info) value))

(defun dbgr-srcbuf-info-cmd-args=(info buffer)
  (setf (dbgr-srcbuf-info-cmd-args info) buffer))

(declare-function fn-p-to-fn?-alias(sym))
(fn-p-to-fn?-alias 'dbgr-srcbuf-info-p)
(declare-function dbgr-srcbuf-info?(var))
(declare-function dbgr-cmdbuf-info-name(cmdbuf-info))

;; FIXME: support a list of cmdprocs's since we want to allow
;; a source buffer to potentially participate in several debuggers
;; which might be active.
(make-variable-buffer-local 'dbgr-srcbuf-info)

(defun dbgr-srcbuf-init 
  (src-buffer cmdproc-buffer debugger-name cmd-args)
  "Initialize SRC-BUFFER as a source-code buffer for a debugger.
CMDPROC-BUFFER is the process-command buffer containing the
debugger.  DEBUGGER-NAME is the name of the debugger.  as a main
program."
  (with-current-buffer cmdproc-buffer
    (set-buffer src-buffer)
    (set (make-local-variable 'dbgr-srcbuf-info)
	 (make-dbgr-srcbuf-info
	  :debugger-name debugger-name
	  :cmd-args cmd-args
	  :cmdproc cmdproc-buffer
	  :loc-hist (make-dbgr-loc-hist)))
    (put 'dbgr-srcbuf-info 'variable-documentation 
	 "Debugger information for a buffer containing source code.")))

(defun dbgr-srcbuf-init-or-update
  (src-buffer cmdproc-buffer)
  "Call `dbgr-srcbuf-init' for SRC-BUFFER update `dbgr-srcbuf-info' variables
in it with those from CMDPROC-BUFFER"
  (let ((debugger-name)
	(cmd-args))
   (with-current-buffer-safe cmdproc-buffer
     (setq debugger-name (dbgr-sget 'cmdbuf-info 'debugger-name))
     (setq cmd-args (dbgr-cmdbuf-info-cmd-args dbgr-cmdbuf-info)))
  (with-current-buffer-safe src-buffer
    (dbgr-populate-common-keys (current-local-map))
    (if (dbgr-srcbuf-info? dbgr-srcbuf-info)
	(progn
	  (dbgr-srcbuf-info-cmdproc= dbgr-srcbuf-info cmdproc-buffer)
	  (dbgr-srcbuf-info-debugger-name= dbgr-srcbuf-info debugger-name)
	  (dbgr-srcbuf-info-cmd-args= dbgr-srcbuf-info cmd-args)
	  )
      (dbgr-srcbuf-init src-buffer cmdproc-buffer "unknown" nil)))))

(defun dbgr-srcbuf-command-string(src-buffer)
  "Get the command string invocation for this source buffer"
  (with-current-buffer-safe src-buffer
    (cond 
     ((and (dbgr-srcbuf? src-buffer)
	   (dbgr-sget 'srcbuf-info 'cmd-args))
      (mapconcat (lambda(x) x) 
		 (dbgr-sget 'srcbuf-info 'cmd-args)
		 " "))
     (t nil))))
  
(provide-me "dbgr-")

