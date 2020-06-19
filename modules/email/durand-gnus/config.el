;;; email/durand-gnus/config.el -*- lexical-binding: t; -*-

(use-package! gnus
  :commands gnus
  :bind (:map durand-view-map
         ("g" . gnus))
  :config
 
  (setf gnus-select-method
        '(nntp "news.gmane.io"))

  (setf gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number))
        gnus-subthread-sort-functions
        'gnus-thread-sort-by-date
        gnus-thread-hide-subtree nil
        gnus-ignored-from-addresses "mmemmew\\.com"))

;;; Gnus-agent

(use-package! gnus-agent
  :after gnus
  :bind (:map gnus-agent-summary-mode-map
         ("$" . previous-line)
         ("ù" . next-line)
         ("n" . gnus-summary-next-article)
         ("p" . gnus-summary-prev-article)
         ("N" . gnus-summary-next-unread-article)
         ("P" . gnus-summary-prev-unread-article)
         ("o" . delete-other-windows)
         ("y" . evil-avy-goto-line)
         ("M-n" . gnus-summary-next-thread)
         ("M-p" . gnus-summary-prev-thread)
         ("C-M-n" . gnus-summary-next-group)
         ("C-M-p" . gnus-summary-prev-group)
         ("z t" . recenter-to-top)
         ("z b" . recenter-to-bottom)
         ("z z" . recenter-to-middle)
         :map gnus-agent-group-mode-map
         ("n" . gnus-group-next-group)
         ("p" . gnus-group-prev-group)
         ("N" . gnus-group-next-unread-group)
         ("P" . gnus-group-prev-unread-group)))

;;; Article mode mappings

(use-package! gnus-art
  :after gnus-agent
  :config
  (define-key gnus-article-mode-map [?z ?t] 'recenter-to-top)
  (define-key gnus-article-mode-map [?z ?b] 'recenter-to-bottom)
  (define-key gnus-article-mode-map [?z ?z] 'recenter-to-middle)
  (define-key gnus-article-mode-map [?o] 'gnus-mime-inline-part)

  (setf gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date))
        gnus-html-frame-width 80
        gnus-inhibit-images t
        gnus-max-image-proportion 0.7))

;;; Summary settings

(use-package! gnus-sum
  :after gnus
  :config
  (setf gnus-auto-select-first nil
        gnus-summary-ignore-duplicates t
        gnus-suppress-duplicates t
        gnus-summary-goto-unread 'never
        gnus-summary-to-prefix "To: "
        gnus-summary-line-format "%U%R%z %-16,16&user-date;  %4L:%-30,30f  %B%s\n"
        gnus-summary-mode-line-format "%p"
        gnus-summary-make-false-root 'adopt
        gnus-sum-thread-tree-false-root "─┬> "
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-leaf-with-other "├─> "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-leaf "└─> "
        gnus-sum-thread-tree-vertical "│"
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject))
