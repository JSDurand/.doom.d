;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; Set C source directory appropariately

(setf find-function-C-source-directory "/Users/durand/w.emacs.d/emacs/src/")

;;; frame title format, just for fun

;; (setf frame-title-format "l'emacs de Durand")
;; NOTE: Silence!
(setf frame-title-format "")

;; (setf frame-title-format '((:eval (let ((bfn (buffer-file-name))
;;                                         (bn (buffer-name)))
;;                                     (cond
;;                                      (bfn)
;;                                      ((string= "*doom*" bn)
;;                                       "DOOM")
;;                                      (t (buffer-name)))))
;;                            " - l'emacs de Durand"))

;;; avy single candidate jump

(setf avy-single-candidate-jump t)

;;; save-place cannot work automatically
(save-place-mode 1)

;;; don't keep screen position
(setf scroll-preserve-screen-position nil)

;;; avy all windows
(setf avy-all-windows 'all-frames)

;;; Prevent some `battery-update' problem.
(fset 'battery-update 'ignore)

;; enable dired details mode
(after! dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;;; Fonts

(setq!
 doom-font "Droid Sans Mono for Powerline-20"
 doom-variable-pitch-font "Avenir" ; inherits `doom-font''s :size
 doom-unicode-font nil
 doom-big-font nil)

;;; NOTE: If the height of `variable-pitch' is an integer, then it will be the
;;; absolute height of the face, which then will not inherit the height of the
;;; default face, and then the command `text-scale-adjust' adjusts only the
;;; height of the default face, thus the face `variable-pitch' will be
;;; unaffected, which is not desired. So we explicitly set it to a floating
;;; number to inherit the height of the default face.
(set-face-attribute 'variable-pitch nil :family "Avenir" :height 1.0)

;;; disable line numbers, as that is a performace killer for me.

(setq-default display-line-numbers-type nil)
(global-display-line-numbers-mode -1)

;;; theme customizations

(setq modus-vivendi-theme-slanted-constructs t
      modus-vivendi-theme-bold-constructs t
      modus-vivendi-theme-3d-modeline t
      modus-vivendi-theme-diffs nil
      modus-vivendi-theme-completions nil
      modus-vivendi-theme-org-blocks 'rainbow
      modus-vivendi-theme-variable-pitch-headings t
      modus-vivendi-theme-rainbow-headings t
      ;; modus-vivendi-theme-section-headings t
      modus-vivendi-theme-scale-headings t
      modus-vivendi-theme-scale-1 1.05
      modus-vivendi-theme-scale-2 1.1
      modus-vivendi-theme-scale-3 1.15
      modus-vivendi-theme-scale-4 1.2
      modus-vivendi-theme-scale-5 1.3)

;;; try modus vivendi theme
(setf doom-theme 'modus-vivendi)
;; (setf doom-theme 'modus-operandi)

;;; mode line help echo
;; (setf mode-line-default-help-echo t)
(tooltip-mode)

;;; option key
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;;; ideal.el
;; (load! "+ideal.el" doom-private-dir)

;; (setq package-enable-at-startup nil)
;; (setq package-archives '(("org" . "http://orgmode.org/elpa/")
;;                          ("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")))
;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)

;; (setf use-package-always-ensure t)
;; +org.el
;; (load! "+org.el" doom-private-dir)

;;; evil-setting.el
;; (load! "evil-setting.el" doom-private-dir)

;;; My own yank-from-kill-ring function

;; (map! :n [?\M-y] 'prot/icomplete-yank-kill-ring)

;;; dollar map is a little strange

(after! org
  (when (keymapp 'durand-evil-dollar-map)
    (map! :map durand-evil-dollar-map
          [?b] (cmd! (recenter -1))
          [?k] 'outline-previous-visible-heading)))

(after! dired
  (map! :map dired-mode-map
        :n [?z ?j] 'dired-next-subdir
        :n [?z ?k] 'dired-prev-subdir))

;;; dictionary mode map

(when (featurep! :tools lookup +dictionary)
  (after! osx-dictionary
    (map! :map osx-dictionary-mode-map
          :n [?q] 'osx-dictionary-quit
          :n [?r] 'osx-dictionary-read-word
          :n [?o] 'osx-dictionary-open-dictionary.app
          :n [?s] 'osx-dictionary-search-input)))

;;; use home key
(map! :meorgvi (kbd "<escape>") #'evil-normal-state
      :mov "à" #'durand-beginning-of-line-or-block
      :n (kbd "<backspace>") #'durand-other-buffer)

;;; some custom mappings

;; archive mapping :ngvm (kbd "s-w") 'delete-other-windows

(map! :map doom-leader-code-map
      "b" 'clean-up-buffers
      "v" #'TeX-view
      "t" #'TeX-command-run-all)

;;; show aux and toc files so that I can delete them

(add-hook! 'dired-load-hook
           (require 'dired-x))

(setq-hook! 'dired-mode-hook
  dired-omit-extensions
  (cl-remove ".aux"
             (cl-remove ".toc" dired-omit-extensions :test 'string=)
             :test 'string=))

;; toggle hl-todo-mode

(map! :map doom-leader-toggle-map
      "h" #'durand-toggle-hl-todo)

;;; transpose word is very important
(map! :n [?\M-t] #'transpose-words
      :map lispyville-mode-map :n [?\M-t] #'transpose-words)

;;; this should be put here in order not to interfere with doom's internal
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
;; (defun load-config ()
;;   "Load the configuration again."
;;   (interactive)
;;   (load! "config.el" doom-private-dir)
;;   (display-battery-mode)
;;   ;; (fset 'pdf-sync-forward-search 'durand-pdf-sync-forward-search)
;;   (setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand))))
;;   (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand)))))

(map! :prefix "g" :m "b" 'evil-goto-line)
;; (map! :n (kbd "s-q") 'load-config)
(map! :n [?\s-q] (cmd! (message "Don't use s-q!")))

;;; set lispy key
(after! (evil-collection lispy)
  (lispy-set-key-theme '(special lispy)))

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width))

;;; message mode quit
(map! :map messages-buffer-mode-map
      :n [?q] 'quit-window)

;; (map! :nvm "s" nil)
;; (map! :g "c" 'self-insert-command)

;;; bookmark remap
(map! :leader :nv (kbd "RET") 'durand-evil-spc-ret-map
      (:map doom-leader-map
        "fp" 'doom/find-file-in-private-config
        "fP" 'doom/open-private-config))

;;; mu4e and elfeed

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

;;; pop up rule for timer list
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

;;; popup rule for org capture
(set-popup-rule! "^CAPTURE.*\\.org$"
  :side 'bottom
  :select t
  :size 0.5
  :quit nil
  :modeline nil)

;;; wrap-region
;; (use-package! wrap-region
;;   :config
;;   (wrap-region-global-mode t)
;;   (wrap-region-add-wrapper "$" "$")
;;   (wrap-region-add-wrapper "=" "=")
;;   (wrap-region-add-wrapper "-" "-"))

;;; default frames behaviour

(setq initial-frame-alist
      '((width . 118)
        (alpha . 90)))
(set-frame-width nil 118)
(set-frame-parameter nil 'alpha 90)
(add-to-list 'default-frame-alist '(width . 118))
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono for Powerline-20"))
(add-to-list 'default-frame-alist '(alpha . 90))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)
(setq revert-without-query '(".*"))

;;; org-agenda and magit and gnus should start with emacs state

(after! evil
  (set-evil-initial-state!
    '(org-agenda-mode
      magit-status-mode
      gnus-group-mode
      gnus-summary-mode
      gnus-article-mode
      gnus-server-mode
      gnus-browse-mode)
    'emacs))

;;; remove org-agenda in motion states
(add-transient-hook! #'durand-agenda
  (setf evil-motion-state-modes (cl-remove 'org-agenda-mode evil-motion-state-modes))
  (when (not (cl-member 'org-agenda-mode evil-emacs-state-modes))
    (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)))

;;; dired should start with the normal state
(set-evil-initial-state!
  '(dired-mode)
  'normal)

;;; modeline config

(setf ;; doom-modeline-height 10
      doom-modeline-enable-word-count nil
      doom-modeline-buffer-encoding nil
      doom-modeline-indent-info nil
      doom-modeline-modal-icon t
      doom-modeline-project-detection 'project
      doom-modeline-mu4e nil)
(setq inhibit-compacting-font-caches t)

(frame-init-behaviour)

(add-hook 'after-make-frame-functions #'frame-init-behaviour)

;; This is not used anymore.
;;;###autoload
;; (defun doom-modeline-update-buffer-narrow-state-icon (&rest _)
;;   "Update the buffer narrowing state in mode-line."
;;   (setq doom-modeline--buffer-narrow-icon
;;         (when doom-modeline-buffer-state-icon
;;           (ignore-errors
;;             (cond ((buffer-narrowed-p)
;;                    (doom-modeline-buffer-file-state-icon
;;                     "vertical_align_center" "↕" "><" 'doom-modeline-warning))
;;                   (t ""))))))


;; NOTE: Don't display narrowing state in the original function. Also, in
;; doom-escape-hook, it stops at the first function that returns non-nil. So I
;; have to wrap it around.
;;

;;; the original function does not work

;; (defun doom-modeline-set-modeline-durand (key &optional default)
;;   "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
;; If DEFAULT is non-nil, set the default mode-line for all buffers."
;;   (when-let ((modeline (doom-modeline key)))
;;     (if default
;;         (setq-default mode-line-format (list "%e" modeline))
;;       (setf (buffer-local-value 'mode-line-format (current-buffer))
;;             (list "%e" modeline)))))

;; (doom-modeline-mode 1)
;; (display-battery-mode 1)

;; (doom-modeline-set-modeline-durand 'durand t)
;; (after! doom-modeline
;;   (add-hook! 'doom-modeline-mode-hook :append
;;     (setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand))))))
;; (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))

;;; pdf view mode mode line
;;;###autoload
;; (defun set-durand-mode-line ()
;;   "Set the mode line to durand style."
;;   (interactive)
;;   (setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))
;;   (force-mode-line-update))

;;; don't ask me if I want to open a file!
(setf large-file-warning-threshold nil)

;;; c++ needs include files
(setq-default flycheck-clang-include-path '("include"))
(setq-default flycheck-gcc-include-path '("include"))

;;; ay-go-to-char
(map! :leader :n "y" #'evil-avy-goto-char-timer)

;;; soft wrap lines
(global-visual-line-mode)

;;; I like my narrow-dwim function

(map! :leader :n :desc "narrow do what I mean" [?'] #'durand-narrow-dwim)

;;; ivy configurations

;; (load! "+ivy.el" doom-private-dir)

;;; I don't like which-key-mode, as it slows down emacs a lot...

(remove-hook 'doom-first-input-hook #'which-key-mode)
(after! which-key  (which-key-mode -1))

;;; load my dashboard configurations

;; (load! "dashboard.el" doom-private-dir)

;;; +default/find-in-notes sometimes crashes because of project detection issues.
;; and no highlights

(map! :map doom-leader-notes-map
      [?n] #'+default/browse-notes
      [?j] 'evil-ex-nohighlight
      :map doom-leader-buffer-map
      [?v] 'ibuffer
      [?b] 'durand-buffers-major-mode
      :leader
      [?,] 'durand-switch-buffer)

(map! :g
      [f5] #'durand-file-size
      [f6] #'show-buffer-name)

;;; pdf viewer

(setf +latex-viewers '(pdf-tools skim))

;;; ispell default dictionary
(setq-default ispell-dictionary "english")

;;; this replaces `save-buffer', but I have my custom key bindings.
(map! :map doom-leader-file-map
      "s" 'durand-save-word)

;;; scratch buffer major mode
(setf doom-scratch-buffer-major-mode 'emacs-lisp-mode)

;;; some temporary binding before the `fold' module supports `outline-minor-mode'
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

;;; glsl auto mode

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

;; delete other windows
(map! :leader :n "wc" #'delete-other-windows)

;; don't watch files since I sometimes have large projects.

(setf lsp-enable-file-watchers nil)

;; problem with helpful mode quit function

(defadvice! +popup-quit-window-a (orig-fn &rest args)
  :around #'quit-window
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (apply orig-fn args)
    (when (and (eq orig-buffer (current-buffer))
               (+popup-window-p))
      (+popup/close)
      (kill-buffer orig-buffer))))

;; prevent terminal specific initialisations
;; (setf term-file-prefix nil)

;;; osxdictionary pop up rule

(set-popup-rule! "\\*osx-dictionary\\*"
  :size 0.25
  :side 'bottom
  :modeline nil
  :ttl 5
  :quit t
  :select t)

;;; don't handle gnus buffers

(set-popup-rule! "\\*\\(Summary\\|Group\\|Article\\|Server\\)"
  :ignore t)


;;; consider doom fallback buffer real

(add-to-list 'doom-real-buffer-functions (lambda (buf) (eq buf (doom-fallback-buffer))) t)

;;; workspace switch to

(when (featurep! :ui workspaces)
  (define-key doom-leader-workspace-map [?t] '+workspace/switch-to))

;;; kill karabiner

(map! :leader :n [?v ?k] 'kill-karabiner)

;; ;; it turns out that I have to kill karabiner every day.

;; (defvar durand-kill-karabiner-timer nil
;;   "A timer that kills karabiner every day.")

;; (setf durand-kill-karabiner-timer
;;       (unless (timerp durand-kill-karabiner-timer)
;;         (let* ((cur-time (decode-time (current-time)))
;;                (cur-year (nth 5 cur-time))
;;                (cur-month (nth 4 cur-time))
;;                (cur-day (nth 3 cur-time))
;;                (cur-hour (nth 2 cur-time)))
;;           (run-with-timer
;;            (float-time
;;             (time-subtract
;;              (cond
;;               ((>= cur-hour 9)
;;                (encode-time 0 0 9 (1+ cur-day) cur-month cur-year))
;;               (t
;;                (encode-time 0 0 9 cur-day cur-month cur-year)))
;;              nil))
;;            ;; a day
;;            (* 24 60 60)
;;            'kill-karabiner))))

;;; interchange pp-eval-expression and eval-expression

(map! [remap pp-eval-expression] 'eval-expression
      [remap eval-expression] 'pp-eval-expression)

;;; remove semantic from completion-at-point-functions

;; (after! semantic
;;   (remove-hook! (semantic-mode emacs-lisp-mode)
;;     (cl-loop for fun in (default-value 'completion-at-point-functions)
;;              when (string-prefix-p "semantic-" (symbol-name fun))
;;              do (remove-hook 'completion-at-point-functions fun))))

;;; it seems to be a better idea to advice rather than remove

(defadvice! semantic-company-a (orig-fun &rest args)
  "Do nothing when inside a string or a comment."
  :around '(semantic-analyze-completion-at-point-function
            semantic-analyze-notc-completion-at-point-function
            semantic-analyze-nolongprefix-completion-at-point-function)
  (when (not (or (nth 3 (syntax-ppss))
                 (nth 4 (syntax-ppss))))
    (apply orig-fun args)))

;;; my durand-cat package

(use-package! durand-cat
  :commands (durand-cat-profile-display-dashboard)
  :bind (:map durand-view-map
          ([?c] . durand-cat-profile-display-dashboard))
  :config
  (map! :map durand-cat-mode-map
        :n [?\(] 'durand-cat-toggle-details
        :n [?s] 'durand-cat-switch-profile
        :n [?q] 'kill-current-buffer
        :n [tab] 'durand-cat-show-every-item
        :map durand-cat-every-item-mode-map
        :n [tab] 'durand-cat-every-item-jump
        :n [?q] 'durand-cat-every-item-quit)
  (define-key! durand-cat-mode-map
    [left-margin mouse-1] #'ignore
    [remap forward-button] #'durand-cat-next-activity
    [remap backward-button] #'durand-cat-previous-activity
    "n" #'durand-cat-down-activity
    "p" #'durand-cat-up-activity
    "C-n" #'durand-cat-down-activity
    "C-p" #'durand-cat-up-activity
    [down] #'durand-cat-down-activity
    [up] #'durand-cat-up-activity
    [left] 'durand-cat-left-activity
    [right] 'durand-cat-right-activity

    ;; Evil remaps
    [remap evil-next-line] #'durand-cat-down-activity
    [remap evil-previous-line] #'durand-cat-up-activity
    [remap evil-next-visual-line] #'durand-cat-down-activity
    [remap evil-previous-visual-line] #'durand-cat-up-activity
    [remap evil-forward-char] #'durand-cat-right-activity
    [remap evil-backward-char] #'durand-cat-left-activity
    [remap evil-paste-pop-next] #'durand-cat-down-activity
    [remap evil-paste-pop] #'durand-cat-up-activity
    [remap evil-delete] #'ignore
    [remap evil-delete-line] #'ignore
    [remap evil-insert] #'ignore
    [remap evil-append] #'ignore
    [remap evil-replace] #'ignore
    [remap evil-replace-state] #'ignore
    [remap evil-change] #'ignore
    [remap evil-change-line] #'ignore
    [remap evil-visual-char] #'ignore
    [remap evil-visual-line] #'ignore))

;;; input method

;; (setf default-input-method "devanagari-itrans")
(map! :map doom-leader-toggle-map
      [?i] 'toggle-input-method)
;;; I am trying pyim now

;; (use-package! pyim
;;   :after-call after-find-file pre-command-hook
;;   :init
;;   (setq pyim-dcache-directory (concat doom-cache-dir "pyim/"))
;;   :config
;;   (setq pyim-page-tooltip t
;;         default-input-method "pyim")

;;   (pyim-basedict-enable)

;;   (map! :map pyim-mode-map
;;         [?,] (cmd! (pyim-page-select-word-by-number 2))
;;         [?\;] (cmd! (pyim-page-select-word-by-number 3))
;;         [?:] (cmd! (pyim-page-select-word-by-number 4))
;;         [?=] (cmd! (pyim-page-select-word-by-number 5))
;;         [?\&] (cmd! (pyim-entered-backward-point))
;;         [?\é] (cmd! (pyim-entered-forward-point))
;;         [?\"] (cmd! (pyim-page-previous-page 1))
;;         [?\'] (cmd! (pyim-page-next-page 1))))

;; REVIEW: But it still too slow for me to use fluently: mainly because of the
;; dearth of a sufficiently large dictionary. It is kind of OK to input Chinese
;; words by pinyin, but it is very important to select Chinese words by phrases,
;; instead of picking them word by word in a million possibilities. Therefore
;; this still needs huge improvements as of now.
;;
;; NOTE: I shall try this out again, since I really cannot stand using the
;; built-in input method on mac, when I use it in emacs...

;; bind indent operator

(map! :n [?g ?i] 'evil-indent)

;; bind go to implementation

(map! :n [?g ?m] '+lookup/implementations)

;; opencc

;; (use-package! opencc
;;   :after pyim
;;   :config
;;   (setf pyim-magic-converter 'sim-2-tw-chinese))

;; bind a key to merge both variants of a conflict without markers

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;;; Bind SPC x to ctl-x-map

(map! :leader
      :n [?x] ctl-x-map)

(map! :map durand-evil-spc-ret-map
      [?x] 'doom/open-scratch-buffer)

;;; I don't want aw-keys to be bound to numbers.

(setf aw-keys (cl-loop for i from 0 to 8
                       collect (+ ?a i)))

;; Bind SPC a to durand-agenda, SPC o t to open terminal, and SPC o s to open
;; browser.

(map! :leader
      :n [?a] 'durand-agenda)

(map! :map doom-leader-open-map
      [?t] 'durand-open-terminal
      [?d] 'durand-open-discord
      [?p] 'durand-open-preferences
      [?s] 'durand-open-browser)

;; bind SPC SPC to `counsel-find-file', and leave `+ivy/projectile-find-file' to
;; SPC p f.

(map! :leader
      :n [32] 'counsel-find-file)

;; bind u to undo-fu

(map! :n [?u] 'undo-fu-only-undo)

;; no persistent undo is fine for me

(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

;; org-noter

(when (featurep! :lang org +noter)
  (setf org-noter-notes-search-path (list "/Users/durand/org/mes-notes/")
        org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-notes-window-location 'vertical-split)

  ;; remap keys
  (map! :map pdf-view-mode-map
        :n [?i] 'org-noter-insert-precise-note
        :n [?\M-n] 'org-noter-sync-next-note
        :n [?\M-p] 'org-noter-sync-prev-note
        :n [?\M-.] 'org-noter-sync-current-note
        :n [?q] 'org-noter-kill-session))

;; deft settings

(use-package! deft
  :commands deft
  :config
  (set-evil-initial-state! 'deft-mode 'emacs)
  (add-to-list 'deft-extensions "org_archive")
  (setf deft-directory org-directory
        deft-recursive t
        deft-recursive-ignore-dir-regexp
        "\\(?:\\.\\|\\.\\.\\|account\\|screen-shots\\|ltximg\\|delf b1.*\\)$"
        deft-ignore-file-regexp
        "ex\\.org$"))

;; biblio use local bibliography files

(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (map! :map doom-leader-notes-map
        [?b] 'ivy-bibtex-with-local-bibliography))


;; rust-analyzer seems to be superior than RLS.

(setq rustic-lsp-server 'rust-analyzer
      lsp-signature-auto-activate nil
      lsp-signature-function 'ignore)

(set-popup-rules!
  '(("*rustic-compilation*"
     :size 0.3
     :select t)
    ("*cargo-test*"
     :size 0.5
     :select t)))

;;; org-agenda start with emacs state!!!

(set-evil-initial-state!
  '(org-agenda-mode)
  'emacs)

;;; my browser of choice

;;;###autoload
(defvar durand-browser "Safari"
  "The browser that I use to browse url.")

;;; don't format in org mode

;; NOTE: I append org-mode here since that list begins with 'not.
(setf +format-on-save-enabled-modes
      (append +format-on-save-enabled-modes
              (list 'org-mode)))

;;; move ibuffer keybindings

(setf ibuffer-expert t
      ibuffer-display-summary nil)

(after! ibuffer
  (map! :map ibuffer-mode-map
        :n [?t] 'ibuffer-toggle-marks
        :n [?~] 'ibuffer-do-toggle-modified
        :n [?/] 'ibuffer-do-kill-lines
        :n [?g ?r] 'ibuffer-update
        :n [?d] 'ibuffer-do-delete
        :n [?D] 'ibuffer-mark-for-delete))

;;; Exit minibuffer with C-j

(when (featurep! :completion durand-completion)
  (map! :map minibuffer-local-map
        [?\C-j] 'exit-minibuffer))

;;; find-file is good enough!

(when (featurep! :completion durand-completion)
  (map! :leader
        [32] 'find-file))

;;; More avy keys

(setq avy-keys (nconc
                (number-sequence ?a ?z)
                (number-sequence ?A ?Z)))

;;; Don't use ido!
(when (featurep! :completion durand-completion)
  (setf projectile-completion-system 'default))

;;; use rg.el when not using ivy.

(use-package! rg
  :commands (rg)
  :when (featurep! :completion durand-completion)
  :config
  (setq rg-group-result t
        rg-hide-command t
        rg-show-columns nil
        rg-show-header t
        rg-custom-type-aliases nil
        rg-default-alias-fallback "all")

  (rg-define-search prot/rg-vc-or-dir
                    "RipGrep in project root or present directory."
                    :query ask
                    :format regexp
                    :files "everything"
                    :dir (let ((vc (vc-root-dir)))
                           (if vc
                               vc                ; search root project dir
                             default-directory)) ; or from the current dir
                    :confirm prefix
                    :flags ("--hidden -g !.git"))

  (rg-define-search prot/rg-ref-in-dir
                    "RipGrep for thing at point in present directory."
                    :query point
                    :format regexp
                    :files "everything"
                    :dir default-directory
                    :confirm prefix
                    :flags ("--hidden -g !.git"))

  (set-popup-rule! "^\\*rg\\*"
    :size 0.3
    :side 'bottom
    :select t
    :ttl 5
    :modeline nil)

  :bind (:map rg-mode-map
         ("s" . prot/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

;;; flyspell dummy is wiser than a dummy popup

(use-package! flyspell-correct
  :commands flyspell-correct-at-point flyspell-correct-previous
  :config
  (when (featurep! :completion durand-completion)
    (setf flyspell-correct-interface 'flyspell-correct-dummy)))

;;; enable flyspell for text modes
(add-hook! '(text-mode-hook) #'flyspell-mode)

;;; Use `completing-read' for company

(when (featurep! :completion durand-completion)
  (map! (:when (featurep! :completion company)
         (:after company
          (:map company-active-map
           "C-S-s" 'durand-company-completing)))))

;;; Why is `rainbow-mode' missing?

(use-package! rainbow-mode
  :commands rainbow-mode
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

;;; company backend for text mode

(set-company-backend! 'text-mode
  '(company-dabbrev company-yasnippet))

;;; toggle mode line

(map! :map doom-leader-toggle-map
      [?m] 'durand-toggle-mode-line)

;; I want to scroll slowly...

(setf mouse-wheel-scroll-amount '(1 ((shift) . 2)))

;; try out xwwp

;; REVIEW: This is actually quite nice to use. Some of the most obvious problems
;; (to me in the past) are all addressed, not perfectly, but acceptably, by this
;; package.
(use-package xwwp-full
  :load-path "~/Desktop/xwwp"
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)
              ("t" . xwwp-ace-toggle)))

;; try out librime to input Chinese in Emacs.

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  (rime-librime-root "~/elisp_packages/librime/dist")
  :config
  (add-to-list 'rime-translate-keybindings "RET")
  (add-to-list 'rime-translate-keybindings "[")
  (add-to-list 'rime-translate-keybindings "]")
  (setf default-input-method "rime"
        rime-show-candidate 'posframe))

;;; screen shot

;; REVIEW: On MacOSX imagemagick does not have x11 enabled, and homebrew
;; officially does not support this feature anymore, so basically I have to
;; build imagemagick from source myself. I have tried to install it by using
;; interactive mode in homebrew, but to no avail. So I guess I will just leave
;; this package as it is here...
(use-package! escr
  :commands (escr-region-screenshot escr-frame-screenshot escr-window-screenshot)
  :config
  (setf escr-screenshot-directory
        (expand-file-name "screenshots" doom-private-dir)))
