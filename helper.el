;;; Miscellaneous utility functions
(require 'load-relative)
(defun fn-p-to-fn?-alias (fn-sym)
  "FN-SYM is assumed to be a symbol which is a function.  If it
ends in a 'p' or '-p', that suffix is stripped; in either case, a
suffix with '?' is added this name is a new alias for that
function FN-SYM."
  (if (and (symbolp fn-sym) (functionp fn-sym))
      (let*
	  ((fn-str (symbol-name fn-sym))
	   (new-fn-str 
	     (cond 
	      ((and (> (length fn-str) 2) (equal "-p" (substring fn-str -2)))
	       (substring fn-str 0 -2))
	      ((and (> (length fn-str) 1) (equal "p" (substring fn-str -1)))
	       (substring fn-str 0 -1))
	      (t fn-str)))
	   (new-fn-sym (intern (concat new-fn-str "?"))))
	(defalias new-fn-sym fn-sym))))

(defun buffer-killed? (buffer)
  "Return t if BUFFER is killed."
  (not (buffer-name buffer)))

(defmacro with-current-buffer-safe (buffer &rest body)
  "Check that BUFFER has not been deleted before calling 
`with-current-buffer'. If it has been deleted return nil."
  (declare (indent 1) (debug t))
  `(if (or (not ,buffer) (buffer-killed? ,buffer))
       nil
     (with-current-buffer ,buffer
       ,@body)))


;; FIXME: prepend dbgr- onto the beginning of struct-symbol
(defmacro dbgr-sget (struct-symbol struct-field) 
  "Simplified access to a field of a `defstruct'
variable. STRUCT-SYMBOL is a defstruct symbol name. STRUCT-FIELD
is a field in that. Access (STRUCT-SYMBOL-STRUCT-FIELD STRUCT-SYMBOL)"
  (declare (indent 1) (debug t))
  `(let* ((dbgr-symbol-str
	   (concat "dbgr-" (symbol-name ,struct-symbol)))
	  (dbgr-field-access
	   (intern (concat dbgr-symbol-str "-" (symbol-name, struct-field)))))
    (funcall dbgr-field-access (eval (intern dbgr-symbol-str)))))

;; FIXME: This is broken. setf is not setting. 
(defmacro dbgr-setter(struct-var field)
  "Create field-setter macro name STRUCT-VAR-FEILD= takes a value parameters and sets 
the FIELD field of STRUCT. The macro created looks like this:
  (defmacro (value) (setf (STRUCT-VAR-FIELD STRUCT-VAR) value))"
  (declare (indent 1) (debug t))
  (let* ((struct-var-field (concat struct-var "-" field))
	  (struct-var-sym (intern struct-var))
	  (struct-var-field-sym (intern struct-var-field))
	  (macro-symbol (intern (concat struct-var-field "="))))
    (list 'defmacro macro-symbol '(value)
       (list 'setf (list struct-var-field-sym struct-var-sym)) 'value)
     ))

;; (defun dbgr-struct-field (var-sym field-sym)
;;   (setq var-str (symbol-name var-sym))
;;   (setq field-str (symbol-name field-sym))
;;   (funcall (symbol-function (intern (concat var-str "-" field-str)))
;; 	   (eval (intern var-str))))

(provide-me "dbgr-")
