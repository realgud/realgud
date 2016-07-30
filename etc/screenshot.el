;;; Prepare a RealGUD screenshot

;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Clément Pit--Claudel

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
;; Run ‘cask exec emacs -Q -L . -l etc/screenshot.el’ from the project's root to
;; build a screenshot.

(defvar my/fringe-width 12)

(defun my/cleanup ()
  (dolist (buffer (buffer-list))
    (kill-buffer buffer)))

(defun my/prepare-UI ()
  "Prepare UI for taking a screenshot."
  (ido-mode)
  (tool-bar-mode)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (fringe-mode (cons my/fringe-width my/fringe-width))
  (blink-cursor-mode -1)
  (setq-default cursor-type 'bar
                split-width-threshold 80
                truncate-partial-width-windows t
                frame-title-format (format "RealGUD:PDB @ Emacs %s" emacs-version)
                x-gtk-use-system-tooltips nil)
  (load-theme 'tango t)
  ;; (set-face-attribute 'tooltip nil :height 60)
  (set-face-attribute 'match nil :background "yellow1")
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 90)
  (set-face-attribute 'mode-line nil :foreground "gray60" :background "black")
  (set-face-attribute 'mode-line-inactive nil :foreground "gray60" :background "#404045")
  (set-face-attribute 'mode-line-buffer-id nil :foreground "#eab700")
  (set-fontset-font t 'unicode "Ubuntu Mono")
  (set-frame-size nil 140 20)
  (redisplay t))

(defun my/load-RealGUD ()
  "Load RealGUD."
  (package-initialize)
  (load-library "realgud")
  (setq realgud-bp-fringe-indicator-style
        '(realgud-bp-filled . realgud-bp-hollow)))

(defvar my/source-buffer nil)
(defvar my/command-buffer nil)

(defun my/load-example ()
  "Prepare an example file and start the debugger."
  (save-window-excursion
    (find-file "realgud/common/fringe-utils.py")
    (setq my/source-buffer (current-buffer))
    (hl-line-mode 1)
    (realgud:pdb (format "python3 -m pdb %S" buffer-file-name))
    (sit-for 3) ;; Give PDB some time to start
    (setq my/command-buffer (current-buffer))
    (font-lock-add-keywords ;; Hide default directory
     nil `((,(regexp-quote default-directory) 0 '(face nil display "<demo>/") append)) t)
    (font-lock-mode 1))
  (switch-to-buffer my/source-buffer)
  (set-window-buffer (split-window-horizontally) my/command-buffer))

(defun my/prepare-screenshot-1 ()
  "Prepare for taking a screenshot."
  (my/prepare-UI)
  (my/load-RealGUD)
  (my/load-example)
  (with-current-buffer my/source-buffer
    (dolist (line '(4 12 17))
      (goto-char (point-min))
      (forward-line (1- line))
      (realgud:cmd-break nil)
      (sit-for 1)))
  (goto-char (point-min))
  (recenter-top-bottom 0)
  (realgud:cmd-next)
  (sit-for 1)
  (realgud:cmd-continue)
  (sit-for 1)
  (hl-line-highlight)
  (set-window-start (get-buffer-window my/command-buffer) 1)
  (message nil))

(print default-directory)
(run-with-idle-timer 0 nil #'my/prepare-screenshot-1)
