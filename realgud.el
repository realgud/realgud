;;; realgud.el --- A modular front-end for interacting with external debuggers

;; Author: Rocky Bernstein
;; Version: 1.2
;; Package-Requires: ((load-relative "1.0") (list-utils "0.4.2") (loc-changes "1.1") (test-simple  "1.0"))
;; URL: http://github.com/rocky/emacs-dbgr
;; Compatibility: GNU Emacs 24.x

;; Copyright (C) 2015 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

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

;; Once upon a time in an Emacs far far away and a programming-style
;; deservedly banished, there was a monolithic Cathederal-like
;; debugger front-end called gub. This interfaced with a number of
;; debuggers, many now dead. Is there anyone still alive that
;; remembers sdb from UNIX/32V circa 1980?
;;
;; This isn't that. Here we make use of more modern programming
;; practices, more numerous and smaller files, unit tests, and better
;; use of emacs primitives, e.g. buffer marks, buffer-local variables,
;; structures, rings, hash tables. Although there is still much to be
;; desired, this code is more scalable and suitable as a common base for
;; an Emacs front-end to modern debuggers.
;;
;; Oh, and because global variables are largely banned, we can support
;; several simultaneous debug sessions.
;;
;; See URL `https://github.com/rocky/emacs-dbgr/wiki/Features' for a list
;; features.

;;
;; The debuggers we currently support are:

;;   NAME           INVOCATION**  WHAT
;    -----------------------------------
;;   bashdb         bashdb         bash
;;   Devel::Trepan  trepan.pl      Perl5
;;   gdb            realgud:gdb    gdb
;;   gub            gub            Go SSA debugger
;;   jdb            realgud:jdb    Java jdb debugger
;;   kshdb          kshdb          Korn Shell 93u+
;;   nodejs         nodejs         node.js javascript debugger
;;   pdb            realgud:pdb    stock C Python debugger
;;   perldb         realgud:perldb stock Perl5 debugger
;;   pydb           pydb           slighly enhanced pdb for Python 2.x
;;   pydbgr         pydbgr         obsolete trepanning debugger for Python 2.x
;;   rb8-trepanning trepan8        MRI Ruby 1.8 and an unpatched YARV 1.9
;;   rbx-trepanning trepanx        trepanning debugger for Rubinius Ruby
;;   remake         remake         GNU Make
;;   ruby-debug     rdebug         Ruby
;;   trepanning     trepan         trepanning debugger for a patched Ruby 1.9
;;   trepanjs       trepanjs       trepanning debugger for nodejs and V8
;;   trepan2        trepan2        trepanning debugger for Python 2.x
;;   trepan3k       trepan3k       trepanning debugger for Python 3.x
;;   zshdb          zshdb          Zsh
;;
;;
;; **gdb, jdb, perldb, pdb invocations require the realgud: preface to
;; disambiguate it from older, preexisting emacs commands in `gud'. The other
;; invocations also accept realgud: prefaces, e.g. realgud:bashdb or
;; realgud:rdebug.  Alas there are older obsolete (i.e. often written by me)
;; Emacs packages out there for bashdb, kshdb, nodejs, pydb, rdebug, zshdb.

;; If you don't see your favorite debugger above, see URL
;; `https://github.com/rocky/emacs-dbgr/wiki/How-to-add-a-new-debugger/'
;; for how you can add your own.

;; The debugger is run out of a comint process buffer, or you can use
;; a `realgud-track-mode' inside an existing shell.

;; To install you will need a couple of other Emacs packages
;; installed. If you install via melpa (`package-install') or
;; `el-get', these will be pulled in automatically. See the
;; installation instructions URL
;; `https://github.com/rocky/emacs-dbgr/wiki/How-to-Install' for all
;; the ways to to install and more details on installation.

;;; Code:

(require 'load-relative)

(defgroup realgud nil
  "The Grand Cathedral Debugger rewrite"
  :group 'processes
  :group 'tools
  :version "24.2")

;; FIXME: extend require-relative for "autoload".
(defun realgud:load-features()
  (require-relative-list
   '(
     "./realgud/common/track-mode"
     "./realgud/common/utils"
     "./realgud/debugger/bashdb/bashdb"
     "./realgud/debugger/gdb/gdb"
     "./realgud/debugger/gub/gub"
     "./realgud/debugger/jdb/jdb"
     "./realgud/debugger/kshdb/kshdb"
     "./realgud/debugger/nodejs/nodejs"
     "./realgud/debugger/pdb/pdb"
     "./realgud/debugger/perldb/perldb"
     "./realgud/debugger/pydb/pydb"
     "./realgud/debugger/pydbgr/pydbgr"
     "./realgud/debugger/rdebug/rdebug"
     "./realgud/debugger/remake/remake"
     "./realgud/debugger/trepan/trepan"
     "./realgud/debugger/trepanjs/trepanjs"
     "./realgud/debugger/trepan.pl/trepanpl"
     "./realgud/debugger/trepan2/trepan2"
     "./realgud/debugger/trepan3k/trepan3k"
     "./realgud/debugger/trepanx/trepanx"
     "./realgud/debugger/trepan8/trepan8"
     "./realgud/debugger/zshdb/zshdb"
     ) "realgud-")
  )

(load-relative "./realgud/common/custom")

(defun realgud-feature-starts-with(feature prefix)
  "realgud-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1))
  (string-prefix-p (symbol-name feature) prefix)
  )

(defun realgud:loaded-features()
  "Return a list of loaded debugger features. These are the
features that start with 'realgud-' and also include standalone debugger features
like 'pydbgr'."
  (let ((result nil)
	(feature-str))
    (dolist (feature features result)
      (setq feature-str (symbol-name feature))
      (cond ((eq 't
		 (string-prefix-p feature-str "realgud-"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 (string-prefix-p feature-str "nodejs"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 (string-prefix-p feature-str "pydbgr"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepan".
		 (string-prefix-p feature-str "trepan"))
	     (setq result (cons feature-str result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepanx".
		 (string-prefix-p feature-str "trepanx"))
	     (setq result (cons feature-str result)))
	    ('t nil))
	)
      )
)

(defun realgud:unload-features()
  "Remove all features loaded from this package. Used in
`realgud:reload-features'. See that."
  (interactive "")
  (let ((result (realgud:loaded-features)))
    (dolist (feature result result)
      (unless (symbolp feature) (setq feature (make-symbol feature)))
      (if (featurep feature)
	(unload-feature feature) 't))
  ))

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

(provide-me)

;;; realgud.el ends here
