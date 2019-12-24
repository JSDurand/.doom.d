;;; editor/durand-evil/config.el -*- lexical-binding: t; -*-

(setf evil-respect-visual-line-mode nil)

(after! (evil-collection lispy)
  (lispy-set-key-theme '(special lispy)))

(after! evil
  (setf evil-normal-state-cursor 'box
        evil-emacs-state-cursor '(bar . 7)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'bar
        evil-motion-state-cursor 'bar
        evil-replace-state-cursor 'hbar
        evil-operator-state-cursor 'hbar)

  (setf evil-cross-lines t)
  (setf evil-emacs-state-modes (delq 'pdf-view-mode evil-emacs-state-modes))
  (add-to-list 'evil-emacs-state-modes 'durand-greek-search-mode)
  (add-to-list 'evil-emacs-state-modes 'account-report-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (setf evil-search-module 'evil-search)
  (setf evil-overriding-maps (delete '(grep-mode-map) evil-overriding-maps))
  (setq evil-normal-state-tag (propertize " N " 'face '((:foreground "gray20" :background "cornflowerblue")))
        evil-emacs-state-tag (propertize " E " 'face '((:foreground "gray20" :background "#39bf4c")))
        evil-insert-state-tag (propertize " I " 'face '((:foreground "gray20" :background "green")))
        evil-replace-state-tag (propertize " R " 'face '((:foreground "gray20" :background "red")))
        evil-motion-state-tag (propertize " M " 'face '((:foreground "gray20" :background "orange")))
        evil-visual-state-tag (propertize " V " 'face '((:foreground "gray20" :background "goldenrod1")))
        evil-operator-state-tag (propertize " O " 'face '((:foreground "gray20" :background "pink"))))
  (evil-define-key nil evil-insert-state-map
    [home] 'evil-normal-state)

  (evil-define-key nil evil-visual-state-map
    [home] 'evil-exit-visual-state)

  (evil-define-key nil evil-replace-state-map
    [home] 'evil-normal-state)

  (evil-define-key nil evil-emacs-state-map
    [home] 'evil-normal-state)

  )

(after! dired
  (define-key dired-mode-map [?\d] nil)
  (define-key dired-mode-map [32] nil)
  (define-key dired-mode-map [?x] nil))

(ignore-errors
  (define-key grep-mode-map [?x] nil)
  (define-key grep-mode-map [32] nil))

;; return map
(define-prefix-command 'durand-evil-ret-map)

(map! :map durand-evil-ret-map
      "\"" 'transpose-chars-back-4
      [?s] 'durand-open-browser
      [?\r] 'durand-open-link
      [?a] 'durand-agenda
      [?d] 'durand-open-discord
      [?t] 'durand-open-terminal)

;; space return map

(define-prefix-command 'durand-evil-spc-ret-map)

(map! :map durand-evil-spc-ret-map
      [?j] 'bookmark-bmenu-list
      [?g] 'revert-buffer
      [?b] 'org-open-bookmarks
      [?B] 'describe-bindings
      [?f] 'counsel-describe-function
      [?k] 'describe-key
      [?v] 'counsel-describe-variable
      [?l] 'org-open-articles
      [?n] 'org-open-novels
      [?y] 'org-open-youtube
      [?d] 'delete-rectangle
      [?c] 'durand-capture
      [?s] 'durand-start-counting
      [?r] 'string-rectangle)

;; $

(define-prefix-command 'durand-evil-dollar-map)

(map! :map durand-evil-dollar-map
      [?\d] 'back-to-indentation
      [?t] (lambda () (interactive) (recenter 0))
      [?z] (lambda () (interactive) (recenter (/ (window-body-height) 2)))
      [?b] (lambda () (interactive) (recenter -1))
      [?£] 'org-retreat)

;; v map
(defvar durand-view-map (make-keymap)
  "The map for space v. Generally the map for viewing things, like
articles, bookmarks, youtube links, novels, or weblinks.")

(map! :map durand-view-map
      [?l] #'org-open-articles
      [?b] #'org-open-bookmarks
      [?y] #'org-open-youtube
      [?w] #'org-open-weblink
      [?W] #'durand-wifi
      [?n] #'org-open-novels
      [?t] #'durand-view-timers
      [?P] #'durand-view-process
      [?p] #'durand-chercher-pdf
      [?v] #'durand-view-videos)

;; space: this is the leader of doom.

(map! :leader
      [tab] 'yas-expand
      ;; [?n] 'evil-ex-nohighlight
      [?=] 'evil-align-regexp
      [?%] 'widen
      [?v] durand-view-map
      [?é] 'split-window-below
      [?\"] 'split-window-right
      ;; [?o] 'durand-new-buffer
      [?O] 'make-blank-space
      ;; [?c] 'clean-up-buffers
      ;; [?C] 'clean-up-buffers-regex
      [?j] 'durand-bookmark-jump-headlong
      [?i ?i] 'durand-open-index
      [?J] 'bookmark-set
      ;; [?r] 'durand-recentf-jump-headlong
      ;; [?g] 'backward-or-up-sexp
      ;; [?h] help-map
      [?-] 'negative-argument
      ;; [?p] evil-projectile-map
      [?\r] durand-evil-spc-ret-map
      ;; [?b] 'durand-switch-buffer
      [?k] 'kill-current-buffer
      [?d] #'durand-mu4e
      ;; [?f] 'counsel-find-file
      ;; try the newer function
      [?'] 'durand-narrow-dwim
      [?z] 'capitalize-region-or-word)

(after! evil
  (map! :m [home] 'evil-normal-state
        ;; [32] durand-evil-space-map
        :m [?\r]  durand-evil-ret-map
        :m [?\d] 'durand-other-buffer
        ;; [?$] durand-evil-dollar-map
        :m [?$] 'evil-end-of-line
        :m [?i] 'evil-emacs-state
        :m [40] 'universal-argument
        ;; [?x] 'amx
        :m [?\)] 'evil-forward-paragraph
        :m [?Q] 'durand-quit-window)

  (map! :n [home] 'evil-emacs-state
        :n [?X] (lambda ()
                  (interactive)
                  (pcase major-mode
                    ('mu4e-headers-mode
                     (mu4e-mark-execute-all t))
                    (_
                     (user-error "Not in mu4e-headers-mode"))))
        :n [backspace] #'durand-other-buffer
        :n [?\d] #'durand-other-buffer
        ;; :n [32] durand-evil-space-map
        :n [?\r] durand-evil-ret-map
        ;; [?\)] 'durand-end-of-line-or-block
        :n [?\)] 'evil-forward-paragraph
        :n [?s] 'durand-general-save-buffer
        :n [f10] 'durand-general-save-buffer
        ;; :n [f9] (lambda () (interactive) (when (functionp durand-tex-action) (funcall durand-tex-action)))
        :n [?U] 'undo-tree-redo
        :n [f12] #'undo-tree-undo
        :n [?t] 'transpose-chars
        :n [?T] 'durand-show-current-time
        :n "&" 'transpose-chars-back-2
        :n "é" 'transpose-chars-back-3
        :n [?\"] 'evil-use-register
        :n [?'] 'fill-paragraph
        :n [?£] 'org-advance
        ;; [?$] durand-evil-dollar-map
        ;; [?$] 'evil-end-of-line
        ;; [?i] 'evil-insert-state
        :n [?j] 'evil-next-visual-line
        :n [?k] 'evil-previous-visual-line
        ;; [?+] 'eval-expression
        ;; "," 'evil-repeat-find-char-reverse
        ;; ";" 'evil-repeat-find-char
        ;; [?,] (lambda () (interactive) (durand-buffer-scroll 'up))
        ;; [?\;] (lambda () (interactive) (durand-buffer-scroll 'down))
        ;; [??] (lambda () (interactive) (durand-buffer-scroll 'up 1))
        ;; [32 ?.] (lambda () (interactive) (durand-buffer-scroll 'down 1)) ; I like the repeat operator after all.
        ;; [?.] 'evil-repeat
        :n [?§] (lambda () (interactive) (evil-scroll-line-down (/ (window-body-height) 2)))
        :n [?è] (lambda () (interactive) (evil-scroll-line-up (/ (window-body-height) 2)))
        :n [?!] (lambda () (interactive) (evil-scroll-line-down 1))
        :n [?ç] (lambda () (interactive) (evil-scroll-line-up 1))
        :n [?Q] 'durand-quit-window
        :n [?p] 'evil-paste-after
        ;; [?\s-m] 'durand-toggle-mode-line
        :n [?ù] 'evil-goto-mark
        ;; [?S] 'cycle-spacing
        ;; [?z] 'downcase-region-or-word
        ;; [?Z] 'upcase-region-or-word
        :n [?z] durand-evil-dollar-map
        :n (kbd "C-SPC") 'set-mark-command
        ;; [?=] 'swiper-isearch
        :n [?=] 'evil-ex-search-forward
        :n [?/] 'counsel-grep-or-swiper
        ;; [?<] 'er/expand-region
        ;; [?>] 'er/contract-region
        ;; (kbd "<S-backspace>") 'durand-next-real-buffer
        ;; (kbd "SPC <S-backspace>") 'durand-previous-real-buffer
        ;; [?-] (lambda () (interactive) (durand-buffer-scroll 'up))
        ;; [?_] (lambda () (interactive) (durand-buffer-scroll 'down))
        ;; [?\C--] (lambda () (interactive) (durand-buffer-scroll 'up 1))
        ;; [?\C-_] (lambda () (interactive) (durand-buffer-scroll 'down 1))
        ;; [?\t] (lambda ()
        ;;         "org-cycle or indent-for-tab-command"
        ;;         (interactive)
        ;;         (cond
        ;;          ((derived-mode-p 'org-mode)
        ;;           (org-cycle current-prefix-arg))
        ;;          (t
        ;;           (indent-for-tab-command current-prefix-arg))))
        :n [?J] 'evil-join
        :n [?\M-o] 'evil-jump-forward
        ;; [?\C-a] 'evil-numbers/inc-at-pt
        ;; [?\C-x] 'evil-numbers/dec-at-pt
        :n [?\(] 'universal-argument
        ;; [?\s-x] (lambda ()
        ;;           (interactive)
        ;;           (setq display-line-numbers
        ;;                 (and (null display-line-numbers)
        ;;                      'relative)))
        :n [?\à] 'durand-beginning-of-line-or-block)

  (map! :map outline-mode-map
        :n "z" durand-evil-dollar-map))

;; universal argument mode
(define-key universal-argument-map [?\(] 'universal-argument-more)

(after! evil
  (when (and (boundp 'define-and-bind-text-object)
             (boundp 'define-and-bind-quote-text-object))
    (define-and-bind-quote-text-object "dollar" "$" ?$)
    (define-and-bind-text-object "slash" "/" "/" "/")
    (define-and-bind-text-object "pipe" "|" "|" "|")
    (define-and-bind-text-object "star" "*" "*" "*")
    (define-and-bind-text-object "frenchquote" "«" "«" "»")
    (define-and-bind-text-object
      "tikz" "z"
      "\\(\\\\bpi\\|\\\\tikzpicture\\).*$"
      "\\(\\\\epi\\|\\\\endtikzpicture\\)")
    (define-and-bind-text-object "environment" "e" "\\\\begin{[^{}]+}$" "\\\\end{[^{}]+}")))
