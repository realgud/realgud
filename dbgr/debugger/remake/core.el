;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))
  
(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core") "dbgr-")
(require-relative-list '("init") "dbgr-remake-")

;; FIXME: I think the following could be generalized and moved to 
;; dbgr-... probably via a macro.
(defvar remake-minibuffer-history nil
  "minibuffer history list for the command `remake'.")

(easy-mmode-defmap remake-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun remake-query-cmdline (&optional opt-debugger)
  (dbgr-query-cmdline 
   'remake-suggest-invocation
   remake-minibuffer-local-map
   'remake-minibuffer-history
   opt-debugger))

;; Convert a command line as would be typed normally to run a script
;; into one that invokes an Emacs-enabled debugging session.
;; "--debugger" in inserted as the first switch.

(defun dbgr-remake-massage-args (command-line)
  (let* ((new-args (list "--debugger"))
	 (args (split-string-and-unquote command-line))
	 (program (car args))
	 (seen-e nil)
	 (shift (lambda ()
	 	  (setq new-args (cons (car args) new-args))
	 	  (setq args (cdr args)))))
    
    ;; Pass all switches and -e scripts through.
    (while (and args
    		(string-match "^-" (car args))
    		(not (equal "-" (car args)))
    		(not (equal "--" (car args))))
      (funcall shift))
    
    (if (or (not args)
    	    (string-match "^-" (car args)))
    	(error "Can't use stdin as the script to debug"))
    ;; This is the program name.
    (funcall shift)
    
    (while args
      (funcall shift))
    
    (nreverse new-args)
    )
  )

(defvar remake-command-name) ; # To silence Warning: reference to free variable

(defun remake-suggest-invocation (debugger-name)
  "Suggest a remake command invocation via `dbgr-suggest-invocaton'"
  (dbgr-suggest-invocation remake-command-name remake-minibuffer-history 
			   "Shell-script" "\\.sh$"))

(defun remake-goto-backtrace-line (pt)
  "Display the location mentioned by the remake backtrace line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "backtrace" dbgr-remake-pat-hash))

(defun remake-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "control-frame" dbgr-remake-pat-hash))

(defun remake-reset ()
  "Remake cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (remake-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*remake-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun remake-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'remake-debugger-support-minor-mode minor-mode-map-alist)
;; 	  remake-debugger-support-minor-mode-map-when-deactive))


(defun remake-customize ()
  "Use `customize' to edit the settings of the `remake' debugger."
  (interactive)
  (customize-group 'remake))

(provide-me "dbgr-remake-")
