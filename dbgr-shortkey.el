(require 'load-relative)
(require-relative-list '("dbgr-helper" "dbgr-custom" "dbgr-key"))

(define-minor-mode dbgr-short-key-mode
  "When enabled, short keys can be used in source buffers in `dbgr'."
  :group 'dbgr
  :global t
  :init-value nil
  ;; body goes here...
)

(defvar dbgr-internal-short-key-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "b" 'dbgr-cmd-break)
    ;; (define-key map "t" 'dbgr-toggle-source-breakpoint-enabled)
    (define-key map [insert] 'dbgr-short-key-mode)
    (dbgr-populate-src-buffer-map-plain map)
    map)
  "Keymap used in `dbgr-internal-short-key-mode'.")

(defvar dbgr-original-read-only nil
  "The value `buffer-read-only' should be restored to after short key mode.")

;; `define-minor-mode' does not set if the mode was on or off prior to
;; being called.
(defvar dbgr-internal-short-key-mode-previous-state nil
  "Used to determine when 'dbgr-internal-short-key-mode' changed state.")

;; Implementation note: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode dbgr-internal-short-key-mode
  "Minor mode with short keys for source buffers for the `dbgr' debugger.
The buffer is read-only when the minor mode is active.

Note that this is for internal use only, please use the global
mode `dbgr-short-key-mode'.

\\{dbgr-internal-short-key-mode-map}"
  :group 'dbgr
  :global nil
  :init-value nil
  :lighter " ShortKeys"
  :keymap dbgr-internal-short-key-mode-map
  (make-local-variable 'dbgr-original-read-only)
  (make-local-variable 'dbgr-internal-short-key-mode-previous-state)
  ;; Ensure action only is performed when the state actually is toggled.
  (unless (eq dbgr-internal-short-key-mode-previous-state
              dbgr-internal-short-key-mode)
    (if dbgr-internal-short-key-mode
        ;; Mode is being turned on.
        (progn
          (setq dbgr-original-read-only buffer-read-only)
          (setq buffer-read-only t))
      ;; Mode is being turned off.
      (setq buffer-read-only dbgr-original-read-only))
    ;; Save the current state, so we can determine when the state is
    ;; toggled in the future.
    (setq dbgr-internal-short-key-mode-previous-state
          dbgr-internal-short-key-mode)))

(defun dbgr-internal-short-key-mode-on ()
  "Turn on `dbgr-internal-short-key-mode' in the current debugger frame."
  (save-current-buffer
    (dbgr-internal-short-key-mode 1)
    ))

(defun dbgr-short-key-mode-maybe-activate ()
  (if dbgr-short-key-mode
      (dbgr-internal-short-key-mode-on)
    (dbgr-internal-short-key-mode-off)))


(defun dbgr-internal-short-key-mode-off ()
  "Turn off `dbgr-internal-short-key-mode' in all buffers."
  (save-current-buffer
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when dbgr-internal-short-key-mode
	  (dbgr-internal-short-key-mode -1)))))

(defun dbgr-populate-src-buffer-map (map)
  "Bind all common keys and menu used in the dbgr src buffers.
This includes the keys bound to `dbgr-key-prefix' (typically C-x
C-a)."
  (dbgr-populate-src-buffer-map-plain map)
  (dbgr-populate-common-keys map)
  (dbgr-populate-debugger-menu map)
  (let ((prefix-map (make-sparse-keymap)))
    (dbgr-populate-src-buffer-map-plain prefix-map)
    (define-key map dbgr-key-prefix prefix-map)))

(provide-me)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-shortkey.el ends here
