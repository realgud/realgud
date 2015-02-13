(require 'load-relative)

;; Really should be part of GNU Emacs. But until then...
(defmacro realgud:string-starts-with(string prefix)
  "compare-strings on STRING anchored from the beginning and up
  to length(PREFIX)"
  (declare (indent 1) (debug t))
  `(compare-strings ,prefix 0 (length ,prefix)
		    ,string  0 (length ,prefix))
  )

(defun realgud:strip (str)
      "Remove leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

;; From http://rosettacode.org/wiki/Flatten_a_list#Emacs_Lisp
(defun realgud:flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (realgud:flatten (car mylist)) (realgud:flatten (cdr mylist))))))

(provide-me "realgud-")
