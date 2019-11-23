;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Fonts

(setq doom-font "DejaVu Sans Mono for Powerline 20"
      doom-variable-pitch-font nil ; inherits `doom-font''s :size
      doom-unicode-font nil
      doom-big-font nil)

;; option key
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; ideal.el
(load! "+ideal.el" doom-private-dir)

(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setf use-package-always-ensure t)
;; +org.el
(load! "+org.el" doom-private-dir)

;; evil-setting.el
(load! "evil-setting.el" doom-private-dir)

;; use home key
(map! :meorgvi [home] #'evil-force-normal-state
      :mov "à" #'evil-digit-argument-or-evil-beginning-of-line
      :n (kbd "<backspace>") #'evil-switch-to-windows-last-buffer)

;; some custom mappings
(use-package! org-pdfview
              :ensure t
              :demand
              :config
              ;; custom store link function to store the height as well
              (defun org-pdfview-store-link ()
                "Store a link to a pdfview buffer."
                (when (eq major-mode 'pdf-view-mode)
                  ;; This buffer is in pdf-view-mode
                  (let* ((path buffer-file-name)
                         (page (pdf-view-current-page))
                         (height (let ((ori (substring-no-properties (pdf-misc-size-indication) 1)))
                                   (cond
                                     ((string= ori "Bot")
                                      "55")
                                     ((string= ori "Top")
                                      nil)
                                     (t
                                       (if (string-match "%%" ori)
                                         (replace-match "" nil nil ori)
                                         ori)))))
                         (real-height (when height
                                        (number-to-string (/ (string-to-number height) 100.0))))
                         (link (concat "pdfview:" path "::" (number-to-string page)
                                       (when height (concat "++" real-height)))))
                    (org-store-link-props
                      :type "pdfview"
                      :link link
                      :description path)))))

(map! :ngvm (kbd "s-w") 'delete-other-windows
      :map doom-leader-code-map "c" 'clean-up-buffers)

;;;###autoload
(defun load-config ()
  "Load the configuration again."
  (interactive)
  (load! "config.el" doom-private-dir)
  (display-battery-mode)
  (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand)))))

(map! :prefix "g" :m "h" 'evil-goto-line)
(map! :n (kbd "s-q") 'load-config)

;; (map! :nvm "s" nil)
;; (map! :g "c" 'self-insert-command)

;; pdf-tools

(setq-default pdf-view-display-size 'fit-width)

;; bookmark remap
(map! :leader :nv (kbd "RET") 'durand-evil-spc-ret-map
      :map doom-leader-map "fp" #'doom/open-private-config)

;; mu4e and elfeed
(load! "mu-el.el" doom-private-dir)

;; pop up rule for timer list
(set-popup-rule! "timer-list"
                 :side 'bottom
                 :quit t
                 :modeline nil
                 :size 0.23)

(set-popup-rule! "current time"
                 :side 'bottom
                 :select t
                 :size 2
                 :quit t
                 :modeline nil)

(map! :map timer-list-mode-map :n "q" #'quit-window)

;; popup rule for org capture
(set-popup-rule! "^CAPTURE.*\\.org$"
  :side 'bottom
  :select t
  :size 0.5
  :quit nil
  :modeline nil)

;; wrap-region
(use-package! wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "$" "$")
  (wrap-region-add-wrapper "=" "=")
  (wrap-region-add-wrapper "-" "-"))

;; default frames behaviour

(setq initial-frame-alist '((width . 118)
                            (alpha . 90)))
(set-frame-width nil 118)
(set-frame-parameter nil 'alpha 90)
(add-to-list 'default-frame-alist '(width . 118))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline 20"))
(add-to-list 'default-frame-alist '(alpha . 90))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)
(setq revert-without-query '(".*"))

;; org-agenda should start with emacs state
(set-evil-initial-state!
  '(org-agenda-mode)
  'emacs)

;; modeline config
(setf doom-modeline-height 30
      doom-modeline-enable-word-count nil
      doom-modeline-buffer-encoding nil
      doom-modeline-indent-info nil
      doom-modeline-evil-state-icon t
      doom-modeline-project-detection 'project
      doom-modeline-mu4e t)
(setq inhibit-compacting-font-caches t)

;;  I want to cut down the buffer name

(require 'doom-modeline)
(defvar durand-buffer-name-max 50)

;;;###autoload
(defun doom-modeline-segment--buffer-info-durand ()
  "Almost the same as `doom-modeline-segment--buffer-info',
but it truncates the buffer name within a fixed length."
  (s-truncate durand-buffer-name-max (doom-modeline-segment--buffer-info) "..."))

(byte-compile 'doom-modeline-segment--buffer-info-durand)

(add-to-list 'doom-modeline-fn-alist (cons 'buffer-info-durand 'doom-modeline-segment--buffer-info-durand))

;;;###autoload
(defun doom-modeline-segment--buffer-position-durand ()
  "Almost the same as `doom-modeline-segment--buffer-position',
except when in `org-agenda-mode' it uses `org-agenda-show-blocks-number' instead."
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (let* ((active (doom-modeline--active))
           (po (org-agenda-show-blocks-number))
           (face (if active 'mode-line 'mode-line-inactive))
           (mouse-face 'mode-line-highlight))
      (concat
       (doom-modeline-spc)
       (doom-modeline-spc)
       (propertize po
                   'face face
                   'help-echo "org agenda block position"
                   'mouse-face mouse-face))))
   (t
    (doom-modeline-segment--buffer-position))))

(byte-compile 'doom-modeline-segment--buffer-position)

(add-to-list 'doom-modeline-fn-alist
             (cons 'buffer-position-durand
                   'doom-modeline-segment--buffer-position-durand))

;; the original function does not work

(defun doom-modeline-set-modeline-durand (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let ((modeline (doom-modeline key)))
    (if default
        (setq-default mode-line-format (list "%e" modeline))
      (setf (buffer-local-value 'mode-line-format (current-buffer))
            (list "%e" modeline)))))

(doom-modeline-def-modeline 'durand
  '(bar
    ;; workspace-name
    ;; window-number
    modals
    matches
    buffer-info-durand
    remote-host
    buffer-position-durand
    ;; parrot
    selection-info)
  '(objed-state
    misc-info
    ;; persp-name
    battery
    ;; grip
    ;; irc
    mu4e
    ;; github
    debug
    ;; lsp
    ;; minor-modes
    input-method
    indent-info
    ;; buffer-encoding
    major-mode
    ;; process
    vcs
    checker))

(doom-modeline-set-modeline-durand 'durand t)
(setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand))))
(setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))

;; pdf view scrolling
(map! (:map pdf-view-mode-map
        :n (kbd "s-m") 'set-durand-mode-line
        :n [?§] 'pdf-view-scroll-up-or-next-page
        :n [?è] 'pdf-view-scroll-down-or-previous-page
        :n [?!] 'evil-collection-pdf-view-next-line-or-next-page
        :n [?ç] 'evil-collection-pdf-view-previous-line-or-previous-page
        :n [?q] 'bury-buffer))

;; pdf view mode mode line
;;;###autoload
(defun set-durand-mode-line ()
  "Set the mode line to durand style."
  (interactive)
  (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))
  (force-mode-line-update))
(add-hook 'pdf-view-mode-hook #'set-durand-mode-line)
;; doom-emacs automatically modifies the mode line format for pdf mode, so I
;; want to stop it.
(setf pdf-view-mode-hook (remq 'doom-modeline-set-pdf-modeline pdf-view-mode-hook))
;; (remove-hook 'pdf-view-mode-hook (lambda () (setf (buffer-local-value mode-line-format (current-buffer))
;;                                                   nil)))

(setf large-file-warning-threshold nil)

;; don't ask me if I want to open a file!
;; c++ needs include files
(setq-default flycheck-clang-include-path '("include"))

;; ay-go-to-char
(map! :leader :n "y" #'evil-avy-goto-char-timer)

;; soft wrap lines
(global-visual-line-mode)

;; I like my narrow-dwim function
(map! :leader :n :desc "narrow do what I mean" [?'] #'durand-narrow-dwim)

;; ivy configurations
(load! "+ivy.el" doom-private-dir)

;; I don't like which-key-mode, as it slows down emacs a lot...
(which-key-mode -1)

;; load my dashboard configurations
(load! "dashboard.el" doom-private-dir)

;; +default/find-in-notes sometimes crashes because of project detection issues.
;; and no highlights
(map! :map doom-leader-notes-map
      [?n] #'+default/browse-notes
      [?j] 'evil-ex-nohighlight)
