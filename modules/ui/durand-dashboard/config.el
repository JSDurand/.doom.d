;;; ui/durand-dashboard/config.el -*- lexical-binding: t; -*-

;; dashboard banner directory
(setf +doom-dashboard-banner-dir (expand-file-name "banners" doom-private-dir))

(setf +doom-dashboard-pwd-policy doom-private-dir)

;; (setf +doom-dashboard-functions '(doom-dashboard-widget-banner
;;                                   doom-dashboard-widget-shortmenu
;;                                   doom-dashboard-widget-loaded
;;                                   doom-dashboard-widget-footer))


(setf +doom-dashboard-functions '(doom-dashboard-widget-banner
                                  doom-dashboard-widget-shortmenu
                                  doom-dashboard-widget-loaded)
      +doom-dashboard-banner-padding '(1 . 3))

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
         :icon (all-the-icons-octicon "browser" :face 'font-lock-keyword-face)
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

(map! :leader :n "vd" #'durand-open-dashboard)
