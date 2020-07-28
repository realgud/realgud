;;; realgud.el --- A modular front-end for interacting with external debuggers -*- lexical-binding: t -*-

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.5.1
;; Package-Type: multi
;; Package-Requires: ((load-relative "1.3.1") (loc-changes "1.2") (test-simple  "1.3.0") (emacs "25"))
;; URL: https://github.com/realgud/realgud/
;; Keywords: debugger, gdb, python, perl, go, bash, zsh, bashdb, zshdb, remake, trepan, perldb, pdb

;; Copyright (C) 2015-2020 Free Software Foundation, Inc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A modular, extensible GNU Emacs front-end for interacting with
;; external debuggers.
;;
;; Quick start: https://github.com/realgud/realgud/
;;
;; See URL `https://github.com/realgud/realgud/wiki/Features' for features, and
;; URL `https://github.com/realgud/realgud/wiki/Debuggers-Supported' for
;; debuggers we can handle.
;;
;; Once upon a time in an Emacs far far away and a programming-style
;; deservedly banished, there was a monolithic Cathederal-like
;; debugger front-end called gud.  This interfaced with a number of
;; debuggers, many now dead.[1]  Is there anyone still alive that
;; remembers sdb from UNIX/32V circa 1980?
;;
;; This isn't that.  Here we make use of more modern programming
;; practices, more numerous and smaller files, unit tests, and better
;; use of Emacs primitives, e.g. buffer marks, buffer-local variables,
;; structures, rings, hash tables.  Although there is still much to be
;; desired, this code is more scalable and suitable as a common base for
;; an Emacs front-end to modern debuggers.
;;
;; Oh, and because global variables are largely banned, we can support
;; several simultaneous debug sessions.

;; RealGUD supports many external debuggers.  See URL
;; `https://github.com/realgud/realgud/wiki/Debuggers-Supported' for a
;; list.  However, if you don't see your favorite debugger, see URL
;; `https://github.com/realgud/realgud/wiki/How-to-add-a-new-debugger/'
;; for how you can add your own.

;; The debugger is run out of a comint process buffer, or you can use
;; a `realgud-track-mode' inside an existing comint shell, or eshell
;; buffer.

;; To install you will need a couple of other Emacs packages
;; installed.  If you install via melpa (`package-install') or
;; `el-get', these will be pulled in automatically.  See the
;; installation instructions URL
;; `https://github.com/realgud/realgud/wiki/How-to-Install' for all
;; the ways to to install and more details on installation.

;; [1] Four or more years in, as of 2018 realgud sports a number of
;; old debuggers too.  However we *mark* them as such, and move them
;; out of the main code base.  See for example:
;; https://github.com/realgud/realgud-old-debuggers.  So that's
;; another difference: this code better *maintained*.

;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))

(require 'load-relative)

(defgroup realgud nil
  "The Grand Cathedral Debugger rewrite"
  :group 'processes
  :group 'tools
  :version "25.1")

;; FIXME: extend require-relative for "autoload".
(defun realgud:load-features()
  (progn
    (require-relative-list
     '(
       "./realgud/common/attach"
       "./realgud/common/track-mode"
       "./realgud/common/backtrack-mode"
       "./realgud/common/breakpoint-mode"
       "./realgud/common/locals-mode"
       "./realgud/common/utils"
       "./realgud/debugger/bashdb/bashdb"
       "./realgud/debugger/gdb/gdb"
       "./realgud/debugger/gub/gub"
       "./realgud/debugger/kshdb/kshdb"
       "./realgud/debugger/pdb/pdb"
       "./realgud/debugger/perldb/perldb"
       "./realgud/debugger/rdebug/rdebug"
       "./realgud/debugger/remake/remake"
       "./realgud/debugger/trepan/trepan"
       "./realgud/debugger/trepanjs/trepanjs"
       "./realgud/debugger/trepan.pl/trepanpl"
       "./realgud/debugger/trepan2/trepan2"
       "./realgud/debugger/trepan3k/trepan3k"
       "./realgud/debugger/zshdb/zshdb"
       ) "realgud-")
    (require-relative-list
     '(
       "./realgud/lang/java"
       "./realgud/lang/js"
       "./realgud/lang/perl"
       "./realgud/lang/posix-shell"
       "./realgud/lang/python"
       "./realgud/lang/ruby"
       ) "realgud-lang-")
    (realgud:loaded-features)
    )
  )

(load-relative "./realgud/common/custom")
(load-relative "./realgud/lang/java")

(defun realgud-feature-starts-with(feature prefix)
  "realgud-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1))
  (string-prefix-p (symbol-name feature) prefix)
  )

(defun realgud:loaded-features()
  "Return a list of loaded debugger features. These are the features
that start with 'realgud-' and 'realgud:'"

  (delq nil
		(mapcar (lambda (x) (and (string-match-p "^\\(realgud:\\|realgud-\\)" (symbol-name x)) x))
				features)))

(defun realgud:unload-features()
  "Remove all features loaded from this package. Used in
`realgud:reload-features'. See that."
  (let ((removal-set (realgud:loaded-features)))
	(dolist (feature removal-set)
	  (unload-feature feature t))
	removal-set)) ; return removed set

(defun realgud:reload-features()
  "Reload all features loaded from this package. Useful if have
changed some code or want to reload another version, say a newer
development version and you already have this package loaded."
  (interactive "")
  (realgud:unload-features)
  (realgud:load-features)
  )

;; Load everything.
(realgud:load-features)


;;; Autoloads-related code

;; This section is needed because package.el doesn't recurse into subdirectories
;; when looking for autoload-able forms.  As a workaround, we statically
;; generate our own autoloads, and force Emacs to read them by adding an extra
;; autoloded form.

;;;###autoload
(defconst realgud--recursive-autoloads-file-name "realgud-recursive-autoloads.el"
  "Where to store autoloads for subdirectory contents.")

;;;###autoload
(defconst realgud--recursive-autoloads-base-directory
  (file-name-directory
   (if load-in-progress load-file-name
     buffer-file-name)))

;;;###autoload
(with-demoted-errors "Error in RealGUD's autoloads: %s"
  (load (expand-file-name realgud--recursive-autoloads-file-name
                          realgud--recursive-autoloads-base-directory)
        t t))

(defun realgud--rebuild-recursive-autoloads ()
  "Update RealGUD's recursive autoloads.
This is needed because the package.el infrastructure doesn't
process autoloads in subdirectories; instead we create an
additional autoloads file of our own, and we load it from an
autoloaded form.  Maintainers should run this after adding
autoloaded functions, and commit the resulting changes."
  (interactive)
  (let ((generated-autoload-file
         (expand-file-name realgud--recursive-autoloads-file-name
                           realgud--recursive-autoloads-base-directory)))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (dolist (name (with-no-warnings
                    (directory-files-recursively
                     realgud--recursive-autoloads-base-directory "" t)))
      (when (file-directory-p name)
        (update-directory-autoloads name)))))

(provide-me)

;;; realgud.el ends here
