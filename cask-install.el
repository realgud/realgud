(require 'cask "~/.cask/cask.el")
(let*
    ((parent-dir
      (if (string-match "test/$" default-directory)
	(file-name-directory (directory-file-name default-directory))
      default-directory)))
  (cask-initialize parent-dir))
;; There is a bug on Travis where we are getting
;; "Symbol’s function definition is void: make-mutex"
;; We'll work around it here
(if (not (functionp 'make-mutex))
    (defun make-mutex(&optional name)))
