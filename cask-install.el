(require 'cask "~/.cask/cask.el")
(let*
    ((parent-dir
      (if (string-match "test/$" default-directory)
	(file-name-directory (directory-file-name default-directory))
      default-directory)))
  (cask-initialize parent-dir))
