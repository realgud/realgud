(require 'load-relative)

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
