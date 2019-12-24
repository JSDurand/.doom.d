;;; tools/durand-pdf/config.el -*- lexical-binding: t; -*-


(after! pdf-view
  (require 'pdf-sync)
  (setq-default pdf-view-display-size 'fit-width)

  (map! (:map pdf-view-mode-map
          :n [return] durand-evil-ret-map
          ;; :n (kbd "s-m") 'set-durand-mode-line
          :n [?§] 'durand-pdf-scroll-up-or-next-page
          :n [?è] 'durand-pdf-scroll-down-or-previous-page
          :n [?!] 'evil-collection-pdf-view-next-line-or-next-page
          :n [?ç] 'evil-collection-pdf-view-previous-line-or-previous-page
          :n [?q] 'bury-buffer))

  (load-file
   "/Users/durand/.emacs.d/.local/straight/repos/evil-collection/modes/pdf/evil-collection-pdf.el"))
