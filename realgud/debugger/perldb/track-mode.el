;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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
;; Stock Perl Debugger "perldb5" tracking a comint or eshell buffer.

(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:perldb-")
(require-relative-list '("../../lang/perl") "realgud-lang-")

(realgud-track-mode-vars "perldb")

(declare-function realgud-perl-populate-command-keys
		  'realgud:perldb)
(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup    realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)

(realgud-perl-populate-command-keys perldb-track-mode-map )

(defun perldb-track-mode-hook()
  (if perldb-track-mode
      (progn
	(use-local-map perldb-track-mode-map)
	(message "using perldb mode map")
	)
    (message "perldb track-mode-hook disable called"))
)

(define-minor-mode perldb-track-mode
  "Minor mode for tracking perl5db source locations inside a process shell via realgud. perl5db is the stock Perl debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{perldb-track-mode-map}
"
  :init-value nil
  ;; :lighter " perldb"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:perldb
  :keymap perldb-track-mode-map

  (realgud:track-set-debugger "perldb")
  (if perldb-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(perldb-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

;; Perldb doesn't have stack switching commands.
(define-key perldb-short-key-mode-map
  [remap realgud:cmd-newer-frame] 'undefined)
(define-key perldb-short-key-mode-map
  [remap realgud:cmd-older-frame] 'undefined)

(provide-me "realgud:perldb-")
