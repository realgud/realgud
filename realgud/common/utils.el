;; Copyright (C) 2016-2018 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
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

(if (or (< emacs-major-version 24)
	(and (= emacs-major-version 24) (<= emacs-minor-version 3)))
    ;; From
    ;; https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-string-into-a-delimited-string
    (defun realgud:join-string (list joiner)
      (mapconcat 'identity list joiner))
  (progn
    (require 'subr-x)
    (defalias 'realgud:join-string 'string-join)))

(defun realgud:canonic-major-mode()
  "Return
    - 'eshell if we are in eshell-mode,
    - 'comint if the major comint-mode or shell-mode
Or raise an error if neither."

  (cond ((derived-mode-p 'eshell-mode)
	 'eshell)
	((derived-mode-p 'comint-mode)
	 'comint)
	('t (error "We can only handle comint, shell, or eshell buffers"))
	))

(defun realgud:remove-ansi-schmutz()
  "Remove ASCII escape sequences that node.js 'decorates' in
prompts and interactive output with"
  (interactive "")
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string "\033\\[[0-9]*[CDGKJhl]" "" output)))
  )


(provide-me "realgud-")
