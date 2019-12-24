;;; tools/durand-pdf/config.el -*- lexical-binding: t; -*-

(after! pdf-tools
  (unless (memq 'pdf-sync-minor-mode pdf-tools-enabled-modes)
    (add-to-list 'pdf-tools-enabled-modes 'pdf-sync-minor-mode)))


(after! (pdf-view evil-collection)
  (require 'pdf-sync)
  (add-transient-hook! 'pdf-view-mode-hook
    ;; (setq-default pdf-view-display-size 'fit-width)

    (map! (:map pdf-view-mode-map
            :n [return] durand-evil-ret-map
            ;; :n (kbd "s-m") 'set-durand-mode-line
            :n [?§] 'durand-pdf-scroll-up-or-next-page
            :n [?è] 'durand-pdf-scroll-down-or-previous-page
            :n [?!] 'evil-collection-pdf-view-next-line-or-next-page
            :n [?ç] 'evil-collection-pdf-view-previous-line-or-previous-page
            :n [?q] 'bury-buffer))

    (evil-collection-require 'pdf)))
