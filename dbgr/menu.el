;; -------------------------------------------------------------------
;; Menu support.
;;

(require 'load-relative)
(require-relative-list '("key") "dbgr-")

;; Note: We want the key binding to show in the menu. However, our
;; situation is a little bit complex:
;;
;; 1) We want the binding of the `common' man (i.e. the function key
;;    the user has selected.)
;;
;; 2) We want this even when the menu is disabled and the key isn't
;;    bound, typically when the debugger isn't running.
;;
;; This has been solved by setting up an explicit ":keys" properly.
(defun dbgr-menu-item (common-map name cmd &rest args)
  "Return a menu item entry with the correct key bindings.

A command can be bound to a number of different key sequences. If
the dbgr common map contains a binding it is displayed in the
menu. (The common map typically contains function key bindings.)"
  (let ((key-binding (where-is-internal cmd (list common-map) t))
        (hint '()))
    (if key-binding
        (setq hint (list :keys (key-description key-binding))))
    (append (list 'menu-item name cmd)
            hint
            args)))


;; Note, we re-populate the menus of the different minor and major
;; modes. The reason is that Emacs caches the key bindings, which
;; means that wrong ones are shown when buffers are changed.

;; Remember, all menu items are added in the reverse order!

(defun dbgr-populate-debugger-menu (map)
  "Populate the 'Debugger' menu."
  (let ((menu (make-sparse-keymap))
        (common-map (make-sparse-keymap)))
    ;; Use a simple common map to find the best key sequence to
    ;; display in menu.
    (dbgr-populate-common-keys common-map)

    (define-key map [menu-bar debugger] (cons "Dbgr" menu))

    ;; (define-key menu [break-delete]
    ;;   (dbgr-menu-item common-map "Enable/disable breakpoint"
    ;;                     'dbgr-toggle-source-breakpoint-enabled
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key menu [break]
    ;;   (dbgr-menu-item common-map "Toggle breakpoint"
    ;;                     'dbgr-toggle-source-breakpoint
    ;;                     :enable '(dbgr-get-process)))

    (define-key menu [cont]
      (dbgr-menu-item common-map "Continue" 'dbgr-cmd-continue
                        :enable '(dbgr-get-process)))

    (define-key menu [finish]
      (dbgr-menu-item common-map "Step out (finish)" 'dbgr-cmd-finish
                        :enable '(dbgr-get-process)))

    (define-key menu [next]
      (dbgr-menu-item common-map "Step over (next)" 'dbgr-cmd-next
                        :enable '(dbgr-get-process)))

    (define-key menu [step]
      (dbgr-menu-item common-map "Step into (step)" 'dbgr-cmd-step
                        :enable '(dbgr-get-process)))

    (define-key map [menu-bar debugger line1] '(menu-item "--"))

    (define-key menu [stop]
      (dbgr-menu-item
       common-map "Stop the debugger" 'dbgr-cmd-quit
       :enable '(dbgr-get-process)))

    (define-key menu [start]
      (dbgr-menu-item common-map "Restart the debugger" 'dbgr-cmd-restart))

    (define-key map [menu-bar debugger line2] '(menu-item "--"))

    ;; ;; --------------------
    ;; ;; The "Options" submenu.

    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key menu [options] (cons "Options" submenu)))

    ;; (define-key map [menu-bar debugger options customize]
    ;;   (dbgr-menu-item common-map
    ;;                     "Customize Dbgr" 'dbgr-customize))

    ;; (define-key map [menu-bar debugger options line1] '(menu-item "--"))



    ;; ----------------
    ;; The "short key" toggle.

    (define-key map [menu-bar debugger options short-key-mode]
      (dbgr-menu-item common-map
                        "Short keys in source" 'dbgr-short-key-mode
                        :button
                        '(:toggle
                          . dbgr-short-key-mode)))

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
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key map [menu-bar debugger view watch]
    ;;   (dbgr-menu-item common-map "Watch" 'dbgr-display-watch-buffer
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key map [menu-bar debugger view stack]
    ;;   (dbgr-menu-item common-map "Stack-Frame trace" 
    ;; 			'dbgr-display-frame-buffer
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key map [menu-bar debugger view shell]
    ;;   (dbgr-menu-item common-map "Debugger Shell" 'dbgr-display-cmd-buffer
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key map [menu-bar debugger view variables]
    ;;   (dbgr-menu-item common-map "Variables" 'dbgr-display-variables-buffer
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key map [menu-bar debugger view breakpoints]
    ;;   (dbgr-menu-item common-map
    ;;                     "Breakpoints" 'dbgr-display-breakpoints-buffer
    ;;                     :enable '(dbgr-get-process)))

    ;; (define-key map [menu-bar debugger view source]
    ;;   (dbgr-menu-item common-map
    ;;                     "Source" 'dbgr-display-source-buffer
    ;;                     :enable '(dbgr-get-process)))
    menu))

(provide-me "dbgr-")
