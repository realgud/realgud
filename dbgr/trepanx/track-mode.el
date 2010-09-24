;;; Ruby "trepanx" Debugger tracking a comint or eshell buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '("../common/track-mode" "../common/cmds" 
			 "../common/menu") "dbgr-")
(require-relative-list '("../common/init/trepanx") "dbgr-init-")
(require-relative-list '("core" "cmds") "trepanx-")

(defvar trepanx-pat-hash)
(defvar trepanx-track-mode nil
  "Non-nil if using trepanx-track mode as a minor mode of some other mode.
Use the command `trepanx-track-mode' to toggle or set this variable.")

(declare-function dbgr-track-mode(bool))

(defun trepanx-track-mode-body()
  "Called when entering or leaving trepanx-track-mode. Variable
`trepanx-track-mode' is a boolean which specifies if we are going
into or out of this mode."
  (dbgr-track-set-debugger "trepanx")
  (dbgr-define-gdb-like-commands)
  (dbgr-define-trepanx-commands)
  (if trepanx-track-mode
      (progn 
 	;; FIXME: until I figure out why this isn't set in the mode
	(local-set-key "\C-c!!"  'trepanx-goto-dollarbang-traceback-line)
        (local-set-key "\C-c!c"  'trepanx-goto-control-frame-line)
        (local-set-key "\C-c!b"  'trepanx-goto-backtrace-line)
	(dbgr-track-mode 't)
	(run-mode-hooks 'trepanx-track-mode-hook))
    (progn 
      (dbgr-track-mode nil)
      (local-unset-key "\C-c!!")
      (local-unset-key "\C-c!c")
      (local-unset-key "\C-c!b"))
    ))

(defvar trepanx-track-mode-map
  (let ((map dbgr-track-mode-map))
    (define-key map [C-c ! !]	'trepanx-goto-dollarbang-backtrace-line)
    (define-key map [C-c ! c]	'trepanx-goto-control-frame-line)
    (define-key map [C-c ! b]	'trepanx-goto-backtrace-line)
    map)
  "Keymap used in `trepanx-track-mode'.")

(define-minor-mode trepanx-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " trepanx"   ;; mode-line indicator from dbgr-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'trepanx
  :keymap trepanx-track-mode-map
  (trepanx-track-mode-body)
)

(provide-me "trepanx-")

;;; Local variables:
;;; eval:(put 'trepan-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; trepanx-track.el ends here
