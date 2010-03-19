;; Fringe marks for history of stopping points

;; Bitmap for hollow overlay-arrow in fringe
;; (define-fringe-bitmap 'hollow-right-triangle
;;  "\xe0\x90\x88\x84\x84\x88\x90\xe0")

;; FIXME: Figure out how to do this as a macro.

(defface dbgr-overlay-arrow1
  '((t
     :foreground "black"
     :weight bold))
  "Fringe face for current position."
  :group 'dbgr)

(defface dbgr-overlay-arrow2
  '((t
     :foreground "gray"
     :weight bold))
  "Fringe face for position one back in fringe."
  :group 'dbgr)

(defface dbgr-overlay-arrow3
  '((t
     :foreground "gainsboro"
     :weight bold))
  "Fringe face for position two back in fringe."
  :group 'dbgr)


(defvar dbgr-overlay-arrow1 nil
  "Overlay arrow variable which contains the most recent debugger
position.")
(defvar dbgr-overlay-arrow2 nil
  "Overlay arrow variable which contains the 2nd most recent debugger
position.")
(defvar dbgr-overlay-arrow3 nil
  "Overlay arrow variable which contains the 3rd most recent debugger
position.")

(eval-when-compile (require 'cl))


;; FIXME: since overlay overlay-arrow-list can be global, and perhaps
;; has to stay that way since some Emacs code may expect that, we
;; should use different global overlay arrow variables for the
;; different debuggers. E.g. rbdbgr-overlay-arrow1,
;; pydbgr-overlay-arrow1 and so on. That way, if those debuggers are
;; running concurrently, the fringe for one doesn't interfere with the
;; fringe for another.

;; Loop to set up fringe position markers. 

;; Here is an example of what each iteration does:
;;
;;   (make-local-variable 'dbgr-overlay-arrow1) ;; or 2, or 3
;;   (put 'dbgr-overlay-arrow1 'overlay-arrow-string "=>" ;; or "2>", or ">3"
;;   (define-fringe-bitmap 'dbgr-overlay-arrow1 "\xc0...")
;;   (add-to-list 'overlay-arrow-variable-list 'dbgr-overlay-arrow1)

(dolist (pair 
	 '( ("3" . "3>")  ("2" . "2>") ("1" . "=>")))
  (let ((arrow-symbol (intern (concat "dbgr-overlay-arrow" (car pair))))
	(arrow-bitmap (intern (concat "dbgr-right-triangle" (car pair))))
	(arrow-face (intern (concat "dbgr-overlay-arrow" (car pair)))))
    (make-local-variable arrow-symbol)
    (put arrow-symbol 'overlay-arrow-string (cdr pair))
    (if (display-images-p)
	(progn
	  (define-fringe-bitmap arrow-bitmap "\xc0\xf0\xf8\xfc\xfc\xf8\xf0\xc0")
	  (put arrow-symbol 'overlay-arrow-bitmap arrow-bitmap)
	  (set-fringe-bitmap-face arrow-bitmap arrow-face)))
    (add-to-list 'overlay-arrow-variable-list arrow-symbol)))

(defun dbgr-fringe-set-arrow (overlay-arrow marker)
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

(defun dbgr-fringe-history-set (loc-hist &optional do-cmdbuf?)
  "Set arrows on the last positions we have stopped on."
  ;; FIXME DRY somehow
  (let* (
	 (loc1 (dbgr-loc-hist-item-at loc-hist 2))
	 (loc2 (dbgr-loc-hist-item-at loc-hist 1))
	 (loc3 (dbgr-loc-hist-item-at loc-hist 0))
	 (mark1 (and loc3 (dbgr-loc-marker loc3)))
	 (mark2 (and loc2 (dbgr-loc-marker loc2)))
	 (mark3 (and loc1 (dbgr-loc-marker loc1)))
	 (cmd-mark1 (and loc3 (dbgr-loc-cmd-marker loc3)))
	 (cmd-mark2 (and loc2 (dbgr-loc-cmd-marker loc2)))
	 (cmd-mark3 (and loc1 (dbgr-loc-cmd-marker loc1)))
	 )
    (if (and loc3 (not (equal mark3 mark2)))
	(progn
	  (dbgr-fringe-set-arrow 'dbgr-overlay-arrow3 mark3)
	  (if do-cmdbuf?
	      (dbgr-fringe-set-arrow 'dbgr-overlay-arrow3 cmd-mark3))))
    (if (and loc2 (not (equal mark2 mark1)))
	(progn 
	  (dbgr-fringe-set-arrow 'dbgr-overlay-arrow2 mark2)
	  (if do-cmdbuf?
	      (dbgr-fringe-set-arrow 'dbgr-overlay-arrow2 cmd-mark2))))
    (if loc1
	(progn
	  (dbgr-fringe-set-arrow 'dbgr-overlay-arrow1 mark1)
	  (if do-cmdbuf?
	      (dbgr-fringe-set-arrow 'dbgr-overlay-arrow1 cmd-mark1))))
    ))

(defun dbgr-fringe-erase-history-arrows ()
  "Erase the history arrows from the fringe. You might want call
this command interactively if you have conceptually stopped
debugging and now find the fringe arrows distracting. But you
don't want to kill the debugger process or quit a debugger
session which should also erase those fringe arrows."
  (interactive)
  (setq dbgr-overlay-arrow1 nil)
  (setq dbgr-overlay-arrow2 nil)
  (setq dbgr-overlay-arrow3 nil))

(provide 'dbgr-fringe)
