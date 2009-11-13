;;; Miscellaneous utility functions
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

(provide 'dbgr-helper)
;; (defun dbgr-struct-field (var-sym field-sym)
;;   (setq var-str (symbol-name var-sym))
;;   (setq field-str (symbol-name field-sym))
;;   (funcall (symbol-function (intern (concat var-str "-" field-str)))
;; 	   (eval (intern var-str))))
