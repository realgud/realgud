(require 'load-relative)
(require 'comint)
(require 'eshell)

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

(defun realgud:canonic-major-mode()
  "Return
    - 'eshell if we are in eshell-mode,
    - 'comint if the major comint-mode or shell-mode
Or raise an error if neither."

  (cond ((eq major-mode 'eshell-mode)
	'eshell)
	((or (eq major-mode 'comint-mode) (eq major-mode 'shell-mode))
	  'comint)
	('t (error "We can only handle comint, shell, or eshell buffers"))
	))

(provide-me "realgud-")
