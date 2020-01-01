;;; completion/durand-ivy/config.el -*- lexical-binding: t; -*-

(after! ivy
  (setf ivy-fixed-height-minibuffer nil
        ;; this format is more confortable
        ivy-count-format "(%d/%d) "
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-format-function 'durand-ivy-format-function-arrow
        counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s"
        ivy-format-functions-alist
        '((counsel-compile-env . counsel-compile-env--format-hint)
          (counsel-colors-web . counsel--colors-web-format-function)
          (counsel-colors-emacs . counsel--colors-emacs-format-function)
          (counsel-evil-registers . counsel--yank-pop-format-function)
          (counsel-yank-pop . counsel--yank-pop-format-function)
          (counsel-git-log . counsel--yank-pop-format-function)
          (counsel-faces . counsel--faces-format-function)
          (swiper-isearch . swiper-isearch-format-function)
          (swiper-multi . swiper--all-format-function)
          (t . durand-ivy-format-function-arrow)))

  (when (boundp 'ivy-re-builders-alist)
    (setq ivy-re-builders-alist
          '((swiper . ivy--regex-ignore-order)
            (swiper-multi . ivy--regex-ignore-order)
            (counsel-rg . ivy--regex-ignore-order)
            (swiper-isearch . ivy--regex-ignore-order)
            (t . ivy--regex-fuzzy))))
  )

;; amx showing keybindings might be too slow
(setq amx-show-key-bindings nil)

;; more avy keys
(setq avy-keys (nconc
                (number-sequence ?a ?z)
                (number-sequence ?A ?Z)))
;; (after! ivy-prescient
;;   (setf
;;    ivy-prescient-sort-commands
;;    (append
;;     ivy-prescient-sort-commands
;;     (list
;;      'durand-bookmark-jump-headlong
;;      'counsel-minibuffer-history
;;      'durand-choose-list))))

;; explicitly set the completing read function for magit
;; (setq magit-completing-read-function 'ivy-completing-read)

;; some ivy-rich functionalities
(after! ivy
	(ivy-set-display-transformer 'durand-switch-buffer nil)
  (when (boundp 'ivy-rich-display-transformers-list)
    (plist-put!
     ivy-rich-display-transformers-list
     'durand-switch-buffer
     '(:columns ((ivy-rich-candidate (:width 30)) ; return the candidate itself
	               (ivy-rich-switch-buffer-size (:width 7)) ; return the buffer size
                 ;; (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                                        ; return the buffer indicators
	               (ivy-rich-switch-buffer-major-mode (:width 12 :face durand-buffer-major-mode-face))
                                        ; return the major mode info
                 ;; (ivy-rich-switch-buffer-project (:width 15 :face success))
                                        ; return project name using `projectile'
	               (ivy-rich-switch-buffer-path
		              (:width (lambda (x)
			                      (truncate-string-to-width
			                       (ivy-rich-switch-buffer-shorten-path
			                        x
			                        (ivy-rich-minibuffer-width 0.4))
			                       (ivy-rich-minibuffer-width 0.4))))))
                                        ; return file path relative to project root or `default-directory' if project is nil
	              :predicate (lambda (cand)
			                       (get-buffer cand))))
    (plist-put!
     ivy-rich-display-transformers-list
     'counsel-describe-function
     '(:columns ((counsel-describe-function-transformer
		              (:width 40))
	               (ivy-rich-counsel-function-docstring
		              (:face font-lock-doc-face :width 0.5)))))
    (plist-put
     ivy-rich-display-transformers-list
     'counsel-find-file
     '(:columns ((ivy-read-file-transformer
		              (:width 0.7))
	               (ivy-file-name-extension
		              (:face warning :width 15)))))
    (plist-put
     ivy-rich-display-transformers-list
     'durand-bookmark-jump-headlong
     '(:columns ((identity
		              (:width 25))
	               (durand-ivy-rich-bookmark-file-path
		              (:face durand-buffer-major-mode-face
		                     :width (lambda (x)
				                          (truncate-string-to-width
				                           (ivy-rich-switch-buffer-shorten-path
				                            x
				                            (ivy-rich-minibuffer-width 0.7))
				                           (ivy-rich-minibuffer-width 0.7))))))))))

(when (featurep! :completion ivy +childframe)
  (after! ivy-posframe
    (cl-loop for fn in '(;; durand-switch-buffer
                         ;; ivy-switch-buffer
                         counsel-describe-function
                         counsel-describe-variable
                         doom/help-search-headings
                         doom/help-packages
                         doom/help-package-homepage
                         counsel-M-x
                         counsel-minibuffer-history)
             do (setf (alist-get fn ivy-posframe-display-functions-alist)
                      nil))))
