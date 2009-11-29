(require 'load-relative)
(require-relative-list '("custom") "dbgr-")

;; -------------------------------------------------------------------
;; Key bindings
;;

(defun dbgr-populate-common-keys-standard (map)
  "Bind the basic debugger key layout used by many debuggers.

\\{dbgr-example-map-standard}"
  (define-key map [f5]    'dbgr-continue)
  ;; (define-key map [S-f5]  'rdebug-quit)
  ;; (define-key map [f9]    'rdebug-toggle-source-breakpoint)
  ;; (define-key map [C-f9]  'rdebug-toggle-source-breakpoint-enabled)
  (define-key map [f10]   'dbgr-next)
  (define-key map [f11]   'dbgr-step)
  ;;(define-key map [S-f11] 'gud-finish)
  )

;; TODO: add eclipse, and netbeans

(defun dbgr-populate-common-keys (map)
  "Define the keys that are used by all debugger buffers, including 
source-code buffers

The variable `dbgr-populate-common-keys-function' controls the layout."
  (define-key map "\C-x\C-a\C-q" 'dbgr-short-key-mode)
  (if dbgr-populate-common-keys-function
      (funcall dbgr-populate-common-keys-function map)))

(defun dbgr-populate-src-buffer-map-plain (map)
  "Bind ordinary text characters used in debugger source-code buffers.

This does not touch change menus; for that see `dbgr-populate-debugger-menu'.
Nor does it touch prefix keys; for that see `dbgr-populate-keys-standard'"
  ;; Keys to view other buffers.
  (let ((prefix-map (make-sparse-keymap)))
    ;; (define-key map "?" 'dbgr-display-secondary-window-help-buffer)
    ;; (define-key map "B" 'dbgr-display-breakpoints-buffer)
    ;; (define-key map "C" 'dbgr-display-cmd-buffer)
    ;; (define-key map "E" 'dbgr-display-error-buffer)
    ;; (define-key map "F" 'dbgr-display-frame-buffer)
    ;; (define-key map "I" 'dbgr-display-info-buffer)
    ;; (define-key map "O" 'dbgr-display-output-buffer)
    ;; (define-key map "S" 'dbgr-display-source-buffer)
    ;; (define-key map "V" 'dbgr-display-variables-buffer)
    ;; (define-key map "W" 'dbgr-display-watch-buffer)
    ;; Common debugger commands.
    (define-key map " " 'dbgr-cmd-step)
    ;; (define-key map "_" 'dbgr-set-stepping-prefix)
    ;; (define-key map "+" 'dbgr-set-stepping-prefix)
    ;; (define-key map "-" 'dbgr-set-stepping-prefix)
    ;; (define-key map "<" 'dbgr-newer-frame)
    ;; (define-key map ">" 'dbgr-older-frame)
    ;; ;; (define-key map "a" 'gud-args)
    ;; ;; (define-key map "b" 'gud-break)
    ;; (define-key map "c" 'dbgr-continue)
    ;; ;; (define-key map "d" 'gud-remove)
    ;; (define-key map "f" 'gud-finish)
    (define-key map "n" 'dbgr-cmd-next)
    ;; (define-key map "p" prefix-map)
    ;; (define-key map "q" 'dbgr-quit)
    ;; (define-key map "r" 'dbgr-restart)
    ;; (define-key map "R" 'dbgr-restart)
    (define-key map "s" 'dbgr-cmd-step)
    (define-key map [M-down]   'dbgr-track-hist-newer)
    (define-key map [M-up]     'dbgr-track-hist-older)
    (define-key map [M-S-down] 'dbgr-track-hist-newest)
    (define-key map [M-S-up]   'dbgr-track-hist-oldest)
    ;; (define-key map [mouse-3]  'dbgr-variables-pretty-print-mouse)
    ;; (define-key prefix-map "l" 'dbgr-print-list-region)
    ;; (define-key prefix-map "p" 'dbgr-pretty-print-region)
    ;; (define-key prefix-map "s" 'dbgr-print-sorted-region)
    ))

(defun dbgr-populate-debugger-menu (map)
  "Populate the Dbgr 'Debugger' menu."
  (let ((menu (make-sparse-keymap))
        (common-map (make-sparse-keymap)))
    ;; ;; Use a simple common map to find the best key sequence to
    ;; ;; display in menu.
    ;; (dbgr-populate-common-keys common-map)

    ;; (define-key map [menu-bar debugger] (cons "Debugger" menu))

    ;; (define-key menu [break-delete]
    ;;   (dbgr-menu-item common-map "Enable/disable breakpoint"
    ;;                     'dbgr-toggle-source-breakpoint-enabled
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key menu [break]
    ;;   (dbgr-menu-item common-map "Toggle breakpoint"
    ;;                     'dbgr-toggle-source-breakpoint
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key menu [finish]
    ;;   (dbgr-menu-item common-map "Step out" 'gud-finish
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key menu [step]
    ;;   (dbgr-menu-item common-map "Step into" 'dbgr-step
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key menu [next]
    ;;   (dbgr-menu-item common-map "Step over" 'dbgr-next
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key menu [cont]
    ;;   (dbgr-menu-item common-map "Continue" 'dbgr-continue
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger line1] '(menu-item "--"))

    ;; (define-key menu [stop]
    ;;   (dbgr-menu-item
    ;;    common-map "Stop the debugger" 'dbgr-quit
    ;;    :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key menu [start]
    ;;   (dbgr-menu-item common-map "Start the debugger" 'dbgr))

    ;; (define-key map [menu-bar debugger line2] '(menu-item "--"))

    ;; ;; --------------------
    ;; ;; The "Options" submenu.

    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key menu [options] (cons "Options" submenu)))

    ;; (define-key map [menu-bar debugger options customize]
    ;;   (dbgr-menu-item common-map
    ;;                     "Customize Dbgr" 'dbgr-customize))

    ;; (define-key map [menu-bar debugger options line1] '(menu-item "--"))



    ;; ;; ----------------
    ;; ;; The "short key" toggle.

    ;; (define-key map [menu-bar debugger options short-key-mode]
    ;;   (dbgr-menu-item common-map
    ;;                     "Short keys in source" 'dbgr-short-key-mode
    ;;                     :button
    ;;                     '(:toggle
    ;;                       . dbgr-short-key-mode)))

    ;; (define-key map [menu-bar debugger options line2] '(menu-item "--"))

    ;; ;; ----------------
    ;; ;; Separate I/O buffer.

    ;; (define-key map [menu-bar debugger options io-buffer]
    ;;   (dbgr-menu-item common-map
    ;;                     "Separate I/O buffer"
    ;;                     'dbgr-toggle-use-separate-io-buffer
    ;;                     :button
    ;;                     '(:toggle
    ;;                       . dbgr-use-separate-io-buffer)))

    ;; ;; --------------------
    ;; ;; The optional secondary windows submenu.


    ;; ;; Placeholder used when populating the menu of the secondary buffers.
    ;; (define-key menu [placeholder] nil)

    ;; ;; --------------------
    ;; ;; The "Window Layout" submenu.
    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key menu [layout] (cons "Window Layout" submenu)))

    ;; ;; ----------------
    ;; ;; The "Window Layout" submenu.


    ;; ;; TODO: The following is a somewhat clumsy implementation. Maybe we can
    ;; ;; automatically generate the entries, or use the `dynamic' menu kind?
    ;; ;;
    ;; ;; Also, there might be other situations where the list might be
    ;; ;; handy, e.g. completion.
    ;; (let ((predefined '(dbgr-window-layout-standard
    ;;                     dbgr-window-layout-no-shell
    ;;                     dbgr-window-layout-conservative
    ;;                     dbgr-window-layout-stack-of-windows
    ;;                     dbgr-window-layout-rocky
    ;;                     dbgr-window-layout-rocky2)))

    ;;   (define-key map [menu-bar debugger layout other]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "Other"
    ;;      'dbgr-set-window-layout
    ;;      :button
    ;;      `(:radio
    ;;        . (not (memq dbgr-window-layout-function (quote ,predefined))))))

    ;;   (define-key map [menu-bar debugger layout rocky]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "Rocky's Own"
    ;;      (lambda ()
    ;;        (interactive)
    ;;        (dbgr-set-window-layout 'dbgr-window-layout-rocky))
    ;;      :button
    ;;      '(:radio
    ;;        . (eq dbgr-window-layout-function
    ;;              'dbgr-window-layout-rocky))))

    ;;   (define-key map [menu-bar debugger layout rocky2]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "Rocky II"
    ;;      (lambda ()
    ;;        (interactive)
    ;;        (dbgr-set-window-layout 'dbgr-window-layout-rocky2))
    ;;      :button
    ;;      '(:radio
    ;;        . (eq dbgr-window-layout-function
    ;;              'dbgr-window-layout-rocky2))))

    ;;   (define-key map [menu-bar debugger layout stack]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "Stack of Windows"
    ;;      (lambda ()
    ;;        (interactive)
    ;;        (dbgr-set-window-layout 'dbgr-window-layout-stack-of-windows))
    ;;      :button
    ;;      '(:radio
    ;;        . (eq dbgr-window-layout-function
    ;;              'dbgr-window-layout-stack-of-windows))))

    ;;   (define-key map [menu-bar debugger layout conservative]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "Conservative"
    ;;      (lambda ()
    ;;        (interactive)
    ;;        (dbgr-set-window-layout 'dbgr-window-layout-conservative))
    ;;      :button
    ;;      '(:radio
    ;;        . (eq dbgr-window-layout-function
    ;;              'dbgr-window-layout-conservative))))

    ;;   (define-key map [menu-bar debugger layout no-shell]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "No Shell"
    ;;      (lambda ()
    ;;        (interactive)
    ;;        (dbgr-set-window-layout 'dbgr-window-layout-no-shell))
    ;;      :button
    ;;      '(:radio
    ;;        . (eq dbgr-window-layout-function
    ;;              'dbgr-window-layout-no-shell))))

    ;;   (define-key map [menu-bar debugger layout standard]
    ;;     (dbgr-menu-item
    ;;      common-map
    ;;      "Standard"
    ;;      (lambda ()
    ;;        (interactive)
    ;;        (dbgr-set-window-layout 'dbgr-window-layout-standard))
    ;;      :button
    ;;      '(:radio
    ;;        . (eq dbgr-window-layout-function
    ;;              'dbgr-window-layout-standard)))))

    ;; (define-key map [menu-bar debugger layout line3] '(menu-item "--"))

    ;; (define-key map [menu-bar debugger layout initial]
    ;;   (dbgr-menu-item common-map
    ;;                     "Restore Debugger Layout"
    ;;                     'dbgr-restore-debugger-window-layout
    ;;                     :enable '(fboundp 'dbgr-restore-debugger-window-layout)))

    ;; (define-key map [menu-bar debugger layout line1] '(menu-item "--"))

    ;; ;; Note: It seems as though :enable doesn't work when :button is used.
    ;; (define-key map [menu-bar debugger layout debugger]
    ;;   (dbgr-menu-item common-map "Current Debugger Layout"
    ;;                     'dbgr-display-debugger-window-configuration
    ;;                     :button
    ;;                     '(:radio
    ;;                       . (eq dbgr-window-configuration-state 'debugger))))

    ;; (define-key map [menu-bar debugger layout original]
    ;;   (dbgr-menu-item common-map "Original Layout"
    ;;                     'dbgr-display-original-window-configuration
    ;;                     :button
    ;;                     '(:radio
    ;;                       . (eq dbgr-window-configuration-state 'original))))

    ;; ;; --------------------
    ;; ;; The "View" submenu.
    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key menu [view] (cons "View" submenu)))

    ;; (define-key map [menu-bar debugger view output]
    ;;   (dbgr-menu-item common-map "Output" 'dbgr-display-output-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger view watch]
    ;;   (dbgr-menu-item common-map "Watch" 'dbgr-display-watch-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger view stack]
    ;;   (dbgr-menu-item common-map "Stack-Frame trace" 
    ;; 			'dbgr-display-frame-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger view shell]
    ;;   (dbgr-menu-item common-map "Debugger Shell" 'dbgr-display-cmd-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger view variables]
    ;;   (dbgr-menu-item common-map "Variables" 'dbgr-display-variables-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger view breakpoints]
    ;;   (dbgr-menu-item common-map
    ;;                     "Breakpoints" 'dbgr-display-breakpoints-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))

    ;; (define-key map [menu-bar debugger view source]
    ;;   (dbgr-menu-item common-map
    ;;                     "Source" 'dbgr-display-source-buffer
    ;;                     :enable '(get-buffer-process gud-comint-buffer)))
    menu))

(provide-me "dbgr-")

