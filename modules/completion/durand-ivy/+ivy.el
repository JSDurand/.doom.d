;; package --- Summary: my ivy customizations

;; don't change ivy height
(setf ivy-fixed-height-minibuffer nil)

;; this format is more confortable
(setq ivy-count-format "(%d/%d) "
      ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected)

(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s %s")

(when (boundp 'ivy-re-builders-alist)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-ignore-order)
          (swiper-multi . ivy--regex-ignore-order)
          (counsel-rg . ivy--regex-ignore-order)
          (t . ivy--regex-fuzzy))))

;; minibuffer color customisation
;; (set-face-foreground 'minibuffer-prompt "#010101")
;; (set-face-background 'minibuffer-prompt "#fafafa")

;; (set-face-foreground 'minibuffer-prompt "goldenrod2")
;; (set-face-background 'minibuffer-prompt "chocolate4")

(when (boundp 'ivy-re-builders-alist)
  (add-to-list 'ivy-re-builders-alist (cons 'swiper-isearch 'ivy--regex-ignore-order)))

;; format function

(defface durand-arrow-face
  '((t
     (:inherit minibuffer-prompt :height 200)))
  "Face for the arrow used by `durand-ivy-format-function-arrow'.")

;;;###autoload
(defun durand-ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer using \"☸\" instead of \">\"."
  (ivy--format-function-generic
   (lambda (str)
     (concat (propertize "☸ "
                         'face
                         'durand-arrow-face)
             ;; '(:foreground "gold" :height 300))
             (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "   " str))
   cands
   "\n"))
(setq ivy-format-function 'durand-ivy-format-function-arrow)

(setf ivy-format-functions-alist
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

;; amx showing keybindings might be too slow
(setq amx-show-key-bindings nil)

;; more avy keys
(setq avy-keys (nconc
                (number-sequence ?a ?z)
                (number-sequence ?A ?Z)))

;; explicitly set the completing read function for magit
(setq magit-completing-read-function 'ivy-completing-read)


;;;###autoload
(defun durand-ivy-rich-bookmark-file-path (file)
  "Show the file path of the bookmark item FILE."
  (bookmark-location file))

;;;###autoload
(defface durand-buffer-major-mode-face
  '((default . (:foreground "light blue")))
  "Face for major mode info in `durand-switch-buffer' function")


;;;###autoload
(defun ivy-file-name-extension (file)
  "Show the extension of the file name.
     This is always a string."
  (cond ((stringp (file-name-extension file))
         (file-name-extension file))
        ((and (null (file-name-extension file))
              (file-directory-p file))
         "dir")
        (t "")))

;; some ivy-rich functionalities
(ivy-set-display-transformer 'durand-switch-buffer nil)
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
                               (ivy-rich-minibuffer-width 0.7))))))))

;; +ivy.el ends here.
