;;; Copyright (C) 2010, 2012, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;; Fringe marks for history of stopping points
(require 'load-relative)
(require-relative-list '("helper") "realgud-")

(declare-function realgud-loc-hist-item-at       'realgud-lochist)
(declare-function buffer-killed?                 'helper)
(declare-function realgud-loc-cmd-marker         'realgud-loc)
(declare-function realgud:loc-follow             'realgud-loc)
(declare-function realgud-loc-marker             'realgud-loc)

;; Bitmap for hollow overlay-arrow in fringe
(if (display-images-p)
    (define-fringe-bitmap 'hollow-right-triangle
      "\xe0\x90\x88\x84\x84\x88\x90\xe0"))

;; FIXME: Figure out how to do this as a macro.

(defface realgud-overlay-arrow1
  '((t
     :foreground "black"
     :weight bold))
  "Fringe face for current position."
  :group 'realgud)

(defface realgud-overlay-arrow2
  '((t
     :foreground "gray"
     :weight bold))
  "Fringe face for position one back in fringe."
  :group 'realgud)

(defface realgud-overlay-arrow3
  '((t
     :foreground "gainsboro"
     :weight bold))
  "Fringe face for position two back in fringe."
  :group 'realgud)


(defvar realgud-overlay-arrow1 nil
  "Overlay arrow variable which contains the most recent debugger
position.")
(defvar realgud-overlay-arrow2 nil
  "Overlay arrow variable which contains the 2nd most recent debugger
position.")
(defvar realgud-overlay-arrow3 nil
  "Overlay arrow variable which contains the 3rd most recent debugger
position.")

(eval-when-compile (require 'cl))


;; FIXME: since overlay overlay-arrow-list can be global, and perhaps
;; has to stay that way since some Emacs code may expect that, we
;; should use different global overlay arrow variables for the
;; different debuggers. E.g. trepan-overlay-arrow1,
;; pyrealgud-overlay-arrow1 and so on. That way, if those debuggers are
;; running concurrently, the fringe for one doesn't interfere with the
;; fringe for another.

;; Loop to set up fringe position markers.

;; Here is an example of what each iteration does:
;;
;;   (make-local-variable 'realgud-overlay-arrow1) ;; or 2, or 3
;;   (put 'realgud-overlay-arrow1 'overlay-arrow-string "=>" ;; or "2>", or ">3"
;;   (define-fringe-bitmap 'realgud-overlay-arrow1 "\xc0...")
;;   (add-to-list 'overlay-arrow-variable-list 'realgud-overlay-arrow1)

(dolist (pair
	 '( ("3" . "3>")  ("2" . "2>") ("1" . "=>")))
  (let ((arrow-symbol (intern (concat "realgud-overlay-arrow" (car pair))))
	(arrow-bitmap (intern (concat "realgud-right-triangle" (car pair))))
	(arrow-face (intern (concat "realgud-overlay-arrow" (car pair)))))
    (make-local-variable arrow-symbol)
    (put arrow-symbol 'overlay-arrow-string (cdr pair))
    (if (display-images-p)
	(progn
	  (define-fringe-bitmap arrow-bitmap "\xc0\xf0\xf8\xfc\xfc\xf8\xf0\xc0")
	  (put arrow-symbol 'overlay-arrow-bitmap arrow-bitmap)
	  (set-fringe-bitmap-face arrow-bitmap arrow-face)))
    (add-to-list 'overlay-arrow-variable-list arrow-symbol)))

(defun realgud-fringe-set-arrow (overlay-arrow marker)
  "Set the fringe indicator or overlay arrow to MARKER. This is done
for example to indicate a debugger position."
  (let ((position (marker-position marker)))
    (if position
	(with-current-buffer (marker-buffer marker)
	  (save-excursion
	    (save-restriction
	      (widen)
	      (progn
		(goto-char position)
		;; We need to ignore field boundaries, so we use
		;; forward-line rather than beginning-of-line.
		(forward-line 0)
		(set overlay-arrow (point-marker)))))))))

(defun realgud-fringe-history-set (loc-hist &optional do-cmdbuf?)
  "Set arrows on the last positions we have stopped on."
  ;; FIXME DRY somehow
  (let* (
	 (loc1 (realgud-loc-hist-item-at loc-hist 2))
	 (loc2 (realgud-loc-hist-item-at loc-hist 1))
	 (loc3 (realgud-loc-hist-item-at loc-hist 0))
	 (mark1 (and loc3 (realgud-loc-marker loc3)))
	 (mark2 (and loc2 (realgud-loc-marker loc2)))
	 (mark3 (and loc1 (realgud-loc-marker loc1)))
	 (cmd-mark1 (and loc3 (realgud-loc-cmd-marker loc3)))
	 (cmd-mark2 (and loc2 (realgud-loc-cmd-marker loc2)))
	 (cmd-mark3 (and loc1 (realgud-loc-cmd-marker loc1)))
	 )
    (when (and loc3 (not (equal mark3 mark2)))
      (realgud-fringe-set-arrow 'realgud-overlay-arrow3 mark3)
      (if do-cmdbuf?
	  (realgud-fringe-set-arrow 'realgud-overlay-arrow3 cmd-mark3)))
    (when (and loc2 (not (equal mark2 mark1)))
      (realgud-fringe-set-arrow 'realgud-overlay-arrow2 mark2)
      (if do-cmdbuf?
	  (realgud-fringe-set-arrow 'realgud-overlay-arrow2 cmd-mark2)))
    (when loc1
      (realgud-fringe-set-arrow 'realgud-overlay-arrow1 mark1)
      (when (and do-cmdbuf? cmd-mark1)
	  (realgud-fringe-set-arrow 'realgud-overlay-arrow1 cmd-mark1)
	  (goto-char (marker-position cmd-mark1)))
      )
    ))

(defun realgud-fringe-erase-history-arrows ()
  "Erase the history arrows from the fringe. You might want call
this command interactively if you have conceptually stopped
debugging and now find the fringe arrows distracting. But you
don't want to kill the debugger process or quit a debugger
session which should also erase those fringe arrows."
  (interactive)
  (setq realgud-overlay-arrow1 nil)
  (setq realgud-overlay-arrow2 nil)
  (setq realgud-overlay-arrow3 nil))

(defun realgud-goto-arrow1()
  "Goto the position stored in realgud-overlay-arrow1"
  (interactive "")
  (if realgud-overlay-arrow1
      (realgud:loc-follow realgud-overlay-arrow1))
  )

(defun realgud-goto-arrow2()
  "Goto the position stored in realgud-overlay-arrow2"
  (interactive "")
  (if realgud-overlay-arrow2
      (realgud:loc-follow realgud-overlay-arrow2))
  )


(defun realgud-goto-arrow3()
  "Goto the position stored in realgud-overlay-arrow3"
  (interactive "")
  (if realgud-overlay-arrow3
      (realgud:loc-follow realgud-overlay-arrow3))
  )

(defun realgud-recenter-arrow1()
  "If the current buffer contains realgud-overlay-arrow1 go to that position"
  (interactive "")
  (if (and realgud-overlay-arrow1
	   (eq (marker-buffer realgud-overlay-arrow1) (current-buffer)))
      (goto-char realgud-overlay-arrow1))
  )

(defun realgud-recenter-arrow(&optional opt-buffer)
  "If the current buffer contains realgud-overlay-arrows 1, 2 or 3
   recenter window to show that"
  (interactive "")
  (let ((buffer (or opt-buffer (current-buffer))))
    ;; We need to update in the order 3..1 so that if there are more than on
    ;; arrows in the same buffer the smaller number (e.g. arrow 1) is the
    ;; position we are at rather than the earlier one (e.g. arrow 3).
    (with-current-buffer-safe buffer
      (if (and realgud-overlay-arrow3
	       (eq (marker-buffer realgud-overlay-arrow3) buffer))
	  (realgud:loc-follow realgud-overlay-arrow3)
	)
      (if (and realgud-overlay-arrow2
	       (eq (marker-buffer realgud-overlay-arrow2) buffer))
	  (realgud:loc-follow realgud-overlay-arrow2)
      )
      (if (and realgud-overlay-arrow1
	       (eq (marker-buffer realgud-overlay-arrow1) buffer))
	  (realgud:loc-follow realgud-overlay-arrow1)
	)
      (redisplay)
      )
    ))


(provide 'realgud-fringe)
