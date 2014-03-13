;;; Copyright (C) 2010-2014 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list '("custom" "key" "menu") "realgud-")
(require-relative-list '("buffer/helper") "realgud-buffer-")

(declare-function realgud-get-cmdbuf  'realgud-buffer-helper)

(defvar realgud-short-key-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (realgud-populate-common-keys map)
    (realgud-populate-src-buffer-map-plain map)
    (realgud-populate-debugger-menu map)
    (define-key map "1"        'realgud-goto-arrow1)
    (define-key map "2"        'realgud-goto-arrow2)
    (define-key map "3"        'realgud-goto-arrow3)
    (define-key map "b"        'realgud-cmd-break)
    (define-key map "c"        'realgud-cmd-continue)
    (define-key map "e"        'realgud-cmd-eval-region)

    ;; FIXME: these can go to a common routine
    (define-key map "<"        'realgud-cmd-newer-frame)
    (define-key map ">"        'realgud-cmd-older-frame)
    (define-key map "d"        'realgud-cmd-newer-frame)
    (define-key map "u"        'realgud-cmd-older-frame)
    (define-key map "l"        'realgud-recenter-arrow)
    (define-key map "B"        'realgud-backtrace-init)
    (define-key map "C"        'realgud-window-cmd-undisturb-src)
    (define-key map "S"        'realgud-window-src-undisturb-cmd)

    (define-key map "R"        'realgud-cmd-restart)
    (define-key map "!"        'realgud-cmd-shell)
    (define-key map [insert]   'realgud-short-key-mode)
    (define-key map [(control x)(control q)] 'realgud-short-key-mode)
    map)
  "Keymap used in `realgud-short-key-mode'.")

;; Implementation note: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode realgud-short-key-mode
  "Minor mode with short keys for source buffers for the `dbgr' debugger.
The buffer is read-only when the minor mode is active.

\\{realgud-short-key-mode-map}"
  :group 'realgud
  :global nil
  :init-value nil
  :lighter " ShortKeys"
  :keymap realgud-short-key-mode-map
  ;; executed on activation/deactivation:
  (realgud-short-key-mode-setup realgud-short-key-mode))

(defun realgud-short-key-mode-setup (mode-on?)
  "Called when entering or leaving `realgud-short-key-mode'. Variable
MODE-ON? a boolean which specifies if we are going into or out of this mode."
  (if (realgud-srcbuf?)
    (let ((cmdbuf (realgud-get-cmdbuf)))
      ;; Ensure action only is performed when the state actually is toggled.
      ;; or when not read-only
      (if (or (not buffer-read-only)
              (not (eq (realgud-sget 'srcbuf-info 'short-key?) mode-on?)))
        (progn
          ;; Save the current state, so we can determine when the
          ;; state is toggled in the future.
          (if (not (eq (realgud-sget 'srcbuf-info 'short-key?) mode-on?))
            (progn
              (realgud-srcbuf-info-short-key?= mode-on?)
              (setq realgud-short-key-mode mode-on?)
              (if mode-on?
                ;; Mode is being turned on.
                (progn
                  (realgud-srcbuf-info-was-read-only?= buffer-read-only)
                  (local-set-key [M-insert] 'realgud-short-key-mode)
                  (when realgud-srcbuf-lock (setq buffer-read-only t))
                  (run-mode-hooks 'realgud-short-key-mode-hook))
                ;; Mode is being turned off: restore read-only state.
                (setq buffer-read-only
                  (realgud-sget 'srcbuf-info 'was-read-only?))))))
    ;; (with-current-buffer-safe cmdbuf
    ;;   (realgud-cmdbuf-info-src-shortkey?= mode-on?)
    ;;   (realgud-cmdbuf-info-in-srcbuf?= mode-on?)
    ;;   )
    ))
    (error "Buffer %s does not seem to be attached to a debugger"
      (buffer-name))))

(defun realgud-short-key-mode-off ()
  "Turn off `realgud-short-key-mode' in all buffers."
  (interactive)
  (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when realgud-short-key-mode
	  (realgud-short-key-mode-setup 0)))))

(defun realgud-populate-src-buffer-map (map)
  "Bind all common keys and menu used in the dbgr src buffers.
This includes the keys bound to `realgud-key-prefix' (typically C-x
C-a)."
  (realgud-populate-src-buffer-map-plain map)
  (realgud-populate-common-keys map)
  (realgud-populate-debugger-menu map)
  (let ((prefix-map (make-sparse-keymap)))
    (realgud-populate-src-buffer-map-plain prefix-map)
    (define-key map realgud-key-prefix prefix-map)))

(provide-me "realgud-")

;;; Local variables:
;;; End:
