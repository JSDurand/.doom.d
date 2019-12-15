;; package --- Summary: My customizations of the dashboard

;;;###autoload
(defun durand-open-terminal ()
  "Open terminal at the current directory."
  (interactive)
  (make-process :name "terminal" :command
                `("open" "-a" "terminal" ,(file-relative-name default-directory))
                :buffer nil))

(setf +doom-dashboard-pwd-policy doom-private-dir)

;; (setf +doom-dashboard-functions '(doom-dashboard-widget-banner
;;                                   doom-dashboard-widget-shortmenu
;;                                   doom-dashboard-widget-loaded
;;                                   doom-dashboard-widget-footer))


(setf +doom-dashboard-functions '(doom-dashboard-widget-banner
                                  doom-dashboard-widget-shortmenu)
      +doom-dashboard-banner-padding '(5 . 5))

(setf +doom-dashboard-menu-sections
      '(;; ("Open articles"
        ;;  :icon (all-the-icons-octicon "book" :face 'font-lock-keyword-face)
        ;;  :when (fboundp 'org-open-articles)
        ;;  :action org-open-articles)
        ;; ("Open Web Links"
        ;;  :icon (all-the-icons-octicon "browser" :face 'font-lock-keyword-face)
        ;;  :when (fboundp 'org-open-weblink)
        ;;  :action org-open-weblink)
        ("Open org-agenda"
         :icon (all-the-icons-octicon "calendar" :face 'font-lock-keyword-face)
         :when (fboundp 'durand-agenda)
         :action durand-agenda)
        ;; ("Recently opened files"
        ;;  :icon (all-the-icons-octicon "file-text" :face 'font-lock-keyword-face)
        ;;  :action recentf-open-files)
        ;; ("Jump to bookmark"
        ;;  :icon (all-the-icons-octicon "bookmark" :face 'font-lock-keyword-face)
        ;;  :action durand-bookmark-jump-headlong)
        ;; ("Open private configuration"
        ;;  :icon (all-the-icons-octicon "tools" :face 'font-lock-keyword-face)
        ;;  :when (file-directory-p doom-private-dir)
        ;;  :action doom/open-private-config)
        ("Open Safari"
         :icon (all-the-icons-material "open_in_browser" :face 'font-lock-keyword-face)
         :when (fboundp 'durand-open-browser)
         :action durand-open-browser)
        ("Open Discord"
         :icon (all-the-icons-material "account_circle" :face 'font-lock-keyword-face)
         :when (fboundp 'durand-open-discord)
         :action durand-open-discord)
        ("Open Terminal"
         :icon (all-the-icons-octicon "terminal" :face 'font-lock-keyword-face)
         :when (fboundp 'durand-open-terminal)
         :action durand-open-terminal)
        ;; ("Search Documentation"
        ;;  :icon (all-the-icons-octicon "book" :face 'font-lock-keyword-face)
        ;;  :action doom/help-search)
        ))

;;;###autoload
(defun durand-open-dashboard ()
  "Open the dash board buffer."
  (interactive)
  (switch-to-buffer (doom-fallback-buffer)))

(map! :leader :n "vd" #'durand-open-dashboard)

;;;###autoload
(defun durand-open-discord (&optional arg)
  "Open Discord.
With ARG \\[universal-argument], close discord."
  (interactive "P")
  (let ((browsing-command (cond ((equal arg '(4))
                                 '("osascript" "-e" "tell application \"Discord\" to quit"))
                                (t
                                 '("open" "-a" "Discord")))))
    (make-process
     :name "Discord"
     :command browsing-command
     :buffer nil))
  (when (equal arg '(4))
    (message "Discord closed.")))
