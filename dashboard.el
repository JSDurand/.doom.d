;; package --- Summary: My customizations of the dashboard

;;;###autoload
(defun durand-open-terminal ()
  "Open terminal at the current directory."
  (interactive)
  (make-process :name "terminal" :command
                `("open" "-a" "terminal" ,(file-relative-name default-directory))
                :buffer nil))

(setf +doom-dashboard-menu-sections
      '(("Open articles"
         :icon (all-the-icons-octicon "book" :face 'font-lock-keyword-face)
         :when (fboundp 'org-open-articles)
         :action org-open-articles)
        ("Open Web Links"
         :icon (all-the-icons-octicon "browser" :face 'font-lock-keyword-face)
         :when (fboundp 'org-open-weblink)
         :action org-open-weblink)
        ("Open org-agenda"
         :icon (all-the-icons-octicon "calendar" :face 'font-lock-keyword-face)
         :when (fboundp 'durand-agenda)
         :action durand-agenda)
        ("Recently opened files"
         :icon (all-the-icons-octicon "file-text" :face 'font-lock-keyword-face)
         :action recentf-open-files)
        ("Jump to bookmark"
         :icon (all-the-icons-octicon "bookmark" :face 'font-lock-keyword-face)
         :action durand-bookmark-jump-headlong)
        ("Open private configuration"
         :icon (all-the-icons-octicon "tools" :face 'font-lock-keyword-face)
         :when (file-directory-p doom-private-dir)
         :action doom/open-private-config)
        ("Open Safari"
         :icon (all-the-icons-material "open_in_browser" :face 'font-lock-keyword-face)
         :when (fboundp 'durand-open-browser)
         :action durand-open-browser)
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
