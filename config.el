;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;* Prevent some `battery-update' problem.
(fset 'battery-update 'ignore)

;; enable dired details mode
(after! dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;;* Fonts

(setq doom-font "DejaVu Sans Mono for Powerline 20"
      doom-variable-pitch-font nil      ; inherits `doom-font''s :size
      doom-unicode-font nil
      doom-big-font nil)

;;* disable line numbers, as that is a performace killer for me.

(setq-default display-line-numbers-type nil)
(global-display-line-numbers-mode -1)

;;* try light theme
;; (setf doom-theme 'doom-solarized-light)

;;* mode line help echo
;; (setf mode-line-default-help-echo t)
(tooltip-mode)

;;* option key
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;;* ideal.el
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

;; (setf use-package-always-ensure t)
;; +org.el
;; (load! "+org.el" doom-private-dir)

;;* evil-setting.el
(load! "evil-setting.el" doom-private-dir)

;;* use home key
(map! :meorgvi [home] #'evil-force-normal-state
      :mov "à" #'durand-beginning-of-line-or-block
      :n (kbd "<backspace>") #'evil-switch-to-windows-last-buffer)

;;* some custom mappings
(use-package! org-pdfview
  ;; :ensure t
  ;; :demand
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
      :map doom-leader-code-map
      "b" 'clean-up-buffers
      "v" #'TeX-view
      "t" #'TeX-command-run-all)

;; toggle hl-todo-mode
;;;###autoload
(defun durand-toggle-hl-todo ()
  "Toggle `hl-todo-mode'."
  (interactive)
  (cond
   (hl-todo-mode
    (hl-todo-mode -1))
   ((not hl-todo-mode)
    (hl-todo-mode 1))))

(map! :map doom-leader-toggle-map
      "h" #'durand-toggle-hl-todo)

;;* transpose word is very important
(map! :n [?\M-t] #'transpose-words
      :map lispyville-mode-map :n [?\M-t] #'transpose-words)

;;* this should be put here in order not to interfere with doom's internal
;; workings
(setq org-highest-priority ?A
      org-lowest-priority ?E
      org-default-priority ?B
      org-agenda-deadline-faces '((0.5 . org-warning)
                                  (0.4 . org-upcoming-deadline)
                                  (0.0 . default))
      org-agenda-block-separator ?\—
      org-pretty-entities t)

;;;###autoload
(defun load-config ()
  "Load the configuration again."
  (interactive)
  (load! "config.el" doom-private-dir)
  (display-battery-mode)
  ;; (fset 'pdf-sync-forward-search 'durand-pdf-sync-forward-search)
  (setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand))))
  (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand)))))

(map! :prefix "g" :m "h" 'evil-goto-line)
;; (map! :n (kbd "s-q") 'load-config)
(map! :n [?\s-q] (lambda! (message "Don't use s-q!")))

;; (map! :nvm "s" nil)
;; (map! :g "c" 'self-insert-command)

;;* pdf-tools

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width))

;;* bookmark remap
(map! :leader :nv (kbd "RET") 'durand-evil-spc-ret-map
      :map doom-leader-map "fp" #'doom/open-private-config)

;;* mu4e and elfeed
(load! "mu-el.el" doom-private-dir)

;;* pop up rule for timer list
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

;;* popup rule for org capture
(set-popup-rule! "^CAPTURE.*\\.org$"
  :side 'bottom
  :select t
  :size 0.5
  :quit nil
  :modeline nil)

;;* wrap-region
(use-package! wrap-region
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "$" "$")
  (wrap-region-add-wrapper "=" "=")
  (wrap-region-add-wrapper "-" "-"))

;;* default frames behaviour

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

;;* org-agenda and magit should start with emacs state


(set-evil-initial-state!
  '(org-agenda-mode magit-status-mode)
  'emacs)

;;* dired should start with the normal state
(set-evil-initial-state!
  '(dired-mode)
  'normal)

;;* modeline config
(setf doom-modeline-height 30
      doom-modeline-enable-word-count nil
      doom-modeline-buffer-encoding nil
      doom-modeline-indent-info nil
      doom-modeline-evil-state-icon t
      doom-modeline-project-detection 'project
      doom-modeline-mu4e t)
(setq inhibit-compacting-font-caches t)

;; when in terminal mode, don't display icons.

;; I need to check for GUI in a hook
;;;###autoload
(cl-defun frame-init-behaviour (&optional (frame (selected-frame)))
  "Disable displaying icons in the mode line when run in a terminal"
  (with-selected-frame frame
    (cond
     ((display-graphic-p nil)
      (setf doom-modeline-icon t))
     (t
      (setf doom-modeline-icon nil)))))

(frame-init-behaviour)

(add-hook 'after-make-frame-functions #'frame-init-behaviour)

;;*  I want to cut down the buffer name

(require 'doom-modeline)
(defvar durand-buffer-name-max 50)

;;;###autoload
(defun doom-modeline-segment--buffer-info-durand ()
  "Almost the same as `doom-modeline-segment--buffer-info',
but it truncates the buffer name within `durand-buffer-name-max'."
  (concat
   (doom-modeline--buffer-narrow-icon-durand)
   (s-truncate durand-buffer-name-max
               (format-mode-line (doom-modeline-segment--buffer-info))
               "...")))

;;* buffer file name style
(setf doom-modeline-buffer-file-name-style 'buffer-name)

(byte-compile 'doom-modeline-segment--buffer-info-durand)

(add-to-list 'doom-modeline-fn-alist (cons 'buffer-info-durand 'doom-modeline-segment--buffer-info-durand))

;; show the narrowing information

;;;###autoload
(defvar doom-modeline--buffer-narrow-icon nil
  "Icon for the narrowing state of the buffer.")

;;;###autoload
(defun doom-modeline-update-buffer-narrow-state-icon (&rest _)
  "Update the buffer narrowing state in mode-line."
  (setq doom-modeline--buffer-narrow-icon
        (when doom-modeline-buffer-state-icon
          (ignore-errors
            (cond ((buffer-narrowed-p)
                   (doom-modeline-buffer-file-state-icon
                    "vertical_align_center" "↕" "><" 'doom-modeline-warning))
                  (t ""))))))

(add-hook 'find-file-hook #'doom-modeline-update-buffer-narrow-state-icon)
;; (add-hook 'after-save-hook #'doom-modeline-update-buffer-narrow-state-icon)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'pop-to-buffer :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'undo :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'undo-tree-undo-1 :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'undo-tree-redo-1 :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'popup-create :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'popup-delete :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'symbol-overlay-rename :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'narrow-to-region :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'durand-narrow-dwim :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'switch-to-buffer :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'kill-buffer :after #'doom-modeline-update-buffer-narrow-state-icon)
(advice-add #'widen :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'org-narrow-to-block :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'org-narrow-to-element :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'org-narrow-to-subtree :after #'doom-modeline-update-buffer-narrow-state-icon)
;; (advice-add #'org-toggle-narrow-to-subtree :after #'doom-modeline-update-buffer-narrow-state-icon)

;; don't display narrowing state in the original function.
;;;###autoload
(defun doom-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line. Modified by Durand."
  (setq doom-modeline--buffer-file-state-icon
        (when doom-modeline-buffer-state-icon
          (ignore-errors
            (cond (buffer-read-only
                   (doom-modeline-buffer-file-state-icon
                    "lock" "🔒" "%1*" `(:inherit doom-modeline-warning
                                                 :weight ,(if doom-modeline-icon
                                                              'normal
                                                            'bold))))
                  ((and buffer-file-name (buffer-modified-p)
                        doom-modeline-buffer-modification-icon)
                   (doom-modeline-buffer-file-state-icon
                    "save" "💾" "%1*" `(:inherit doom-modeline-buffer-modified
                                                 :weight ,(if doom-modeline-icon
                                                              'normal
                                                            'bold))))
                  ((and buffer-file-name
                        (not (file-exists-p buffer-file-name)))
                   (doom-modeline-buffer-file-state-icon
                    "block" "🚫" "!" 'doom-modeline-urgent))
                  (t ""))))))

;;;###autoload
(defsubst doom-modeline--buffer-narrow-icon-durand ()
  "The icon of the current narrowing state."
  (when doom-modeline-buffer-state-icon
    (when-let ((icon (or doom-modeline--buffer-narrow-icon
                         (doom-modeline-update-buffer-narrow-state-icon))))
      (concat
       (if (doom-modeline--active)
           icon
         (propertize icon 'face `(:inherit ,(get-text-property 0 'face icon)
                                           :inherit mode-line-inactive)))
       (doom-modeline-vspc)))))

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

(byte-compile 'doom-modeline-segment--buffer-position-durand)

(add-to-list 'doom-modeline-fn-alist
             (cons 'buffer-position-durand
                   'doom-modeline-segment--buffer-position-durand))

;;* the original function does not work

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
    ;; selection-info
    )
  '(objed-state
    misc-info
    ;; persp-name
    battery
    ;; grip
    ;; irc
    mu4e
    ;; github
    debug
    lsp
    ;; minor-modes
    input-method
    indent-info
    ;; buffer-encoding
    major-mode
    ;; process
    vcs
    checker))
  
(doom-modeline-mode 1)
(display-battery-mode 1)

;; (doom-modeline-set-modeline-durand 'durand t)
(after! doom-modeline
  (add-hook! 'doom-modeline-mode-hook :append
    (setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand))))))
;; (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))

;;* pdf view scrolling

;;;###autoload
(defun durand-pdf-scroll-up-or-next-page ()
  "Scroll half a page instead of nearly a page."
  (interactive)
  (durand-buffer-scroll 'up nil nil))

;;;###autoload
(defun durand-pdf-scroll-down-or-previous-page ()
  "Scroll half a page instead of nearly a page."
  (interactive)
  (durand-buffer-scroll 'down nil nil))

(map! (:map pdf-view-mode-map
        :n [return] durand-evil-ret-map
        :n (kbd "s-m") 'set-durand-mode-line
        :n [?§] 'durand-pdf-scroll-up-or-next-page
        :n [?è] 'durand-pdf-scroll-down-or-previous-page
        :n [?!] 'evil-collection-pdf-view-next-line-or-next-page
        :n [?ç] 'evil-collection-pdf-view-previous-line-or-previous-page
        :n [?q] 'bury-buffer))

;;* pdf view mode mode line
;;;###autoload
(defun set-durand-mode-line ()
  "Set the mode line to durand style."
  (interactive)
  (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))
  (force-mode-line-update))

;;;###autoload
(defun set-nil-mode-line ()
  "Disable mode line."
  (interactive)
  (setf mode-line-format nil)
  (force-mode-line-update))

(add-hook 'pdf-view-mode-hook #'set-nil-mode-line)
;;* doom-emacs automatically modifies the mode line format for pdf mode, so I
;; want to stop it.
(setf pdf-view-mode-hook (remq 'doom-modeline-set-pdf-modeline pdf-view-mode-hook))
;; (remove-hook 'pdf-view-mode-hook (lambda () (setf (buffer-local-value mode-line-format (current-buffer))
;;                                                   nil)))

;;* don't ask me if I want to open a file!
(setf large-file-warning-threshold nil)

;;* c++ needs include files
(setq-default flycheck-clang-include-path '("include"))
(setq-default flycheck-gcc-include-path '("include"))

;;* ay-go-to-char
(map! :leader :n "y" #'evil-avy-goto-char-timer)

;;* soft wrap lines
(global-visual-line-mode)

;;* I like my narrow-dwim function
(map! :leader :n :desc "narrow do what I mean" [?'] #'durand-narrow-dwim)

;;* ivy configurations
;; (load! "+ivy.el" doom-private-dir)

;;* I don't like which-key-mode, as it slows down emacs a lot...
(which-key-mode -1)

;;* load my dashboard configurations
(load! "dashboard.el" doom-private-dir)

;;* +default/find-in-notes sometimes crashes because of project detection issues.
;; and no highlights
(map! :map doom-leader-notes-map
      [?n] #'+default/browse-notes
      [?j] 'evil-ex-nohighlight)

;;* count the size of a buffer
;;;###autoload
(defun durand-file-size (&optional arg)
  "Show the buffer size in echo area.
If ARG is non-nil, show raw file size;
if ARG is nil, then show human-readable format."
  (interactive "P")
  (message
   "%s"
   (cond
    (arg
     (- (point-max) (point-min)))
    (t
     (file-size-human-readable
      (- (point-max) (point-min)))))))

;;;###autoload
(defun show-buffer-name (&optional arg)
  "Show the name of the buffer in echo area.
If ARG is non-nil, show the full name of the buffer."
  (interactive "P")
  (cond
   (arg
    (message (buffer-file-name)))
   (t
    (message (buffer-name)))))

(map! :g
      [f5] #'durand-file-size
      [f6] #'show-buffer-name)

;;* pdf viewer
(setf +latex-viewers '(pdf-tools skim))

;;* ispell default dictionary
(setq-default ispell-dictionary "english")

;;* dashboard banner directory
(setf +doom-dashboard-banner-dir (expand-file-name "banners" doom-private-dir))

;;* flyspell save a word to the current dictionary
;;;###autoload
(defun durand-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (cond
     ((consp word)
      (flyspell-do-correct
       'save
       nil
       (car word)
       current-location
       (cadr word)
       (caddr word)
       current-location))
     (t
      (message "No word to save.")))))

;;* this replaces `save-buffer', but I have my custom key bindings.
(map! :map doom-leader-file-map
      "s" 'durand-save-word)

;;* scratch buffer major mode
(setf doom-scratch-buffer-major-mode 'emacs-lisp-mode)

;;* some temporary binding before the `fold' module supports `outline-minor-mode'
;; correctly.

(map!
 :prefix [?z]
 :n [?o] 'outline-show-all
 :n [?m] 'outline-hide-body
 :n [?a] 'outline-cycle
 :n [?j] 'outline-next-visible-heading
 :n [?k] 'outline-previous-visible-heading)

;; hl-todo for LaTeX
(add-hook 'LaTeX-mode-hook #'hl-todo-mode t)

;; compile and run in CC mode

(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map [f8] 'compile-and-run-c)
     (define-key c++-mode-map [f8] 'compile-and-run-c)))

(defvar c-program-name "./main"
  "The executable name to run after compilation.")

(make-variable-buffer-local 'c-program-name)

;;;###autoload
(defun compile-and-run-c (&optional arg)
  "Compile and run in c-mode"
  (interactive "P")
  (if (null arg)
      (make-process
       :name "*compilation*"
       :buffer "*C compilation*"
       :command '("make" "main")
       :sentinel (lambda (_process event-string)
                   (if (string-prefix-p "finished" event-string)
                       (let ((default-directory (file-name-directory (buffer-file-name))))
                         (make-process
                          :name "run"
                          :buffer "*running*"
                          :command (list (concat (file-name-as-directory default-directory) c-program-name))))
                     (user-error "There is a problem!")
                     (switch-to-buffer "C compilation"))))
    (make-process
     :name "*Instruments*"
     :buffer nil
     :command '("open" "/Applications/Xcode.app/Contents/Applications/Instruments.app"))))


;;* glsl auto mode

(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.gs\\'" . glsl-mode))

;; ccls

(after! ccls
  (setq ccls-initialization-options
        `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                    "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                    "-isystem/usr/local/include"]
                        :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir"))))))
