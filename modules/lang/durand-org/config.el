;;; lang/durand-org/config.el -*- lexical-binding: t; -*-

;; use amsfont in latex preview
(setf org-latex-packages-alist '(("" "amsfonts" t)))
(ignore-errors (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))
;; (require 'org-tempo)

;; NOTE: Necessary since org-mode 9.2
(use-package! org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("g" . "src durand-greek"))
  (setf (alist-get "el" org-structure-template-alist nil nil 'string=)
        "src emacs-lisp"))
;; (after! org-tempo
;;   (add-to-list 'org-structure-template-alist '("g" . "src durand-greek")))
(after! org
  (add-to-list 'org-modules 'ol-gnus)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "START(s)" "WORKING(w)" "HARD-WORKING(h)" "ALMOST(a)" "|" "DONE(d)")
          (sequence "TO-THINK(c)" "PENDING(p)" "HARD(r)" "IMPOSSIBLE(i)" "|" "SOLVED(v)"))
        org-tags-column -110
        org-hide-emphasis-markers nil
        org-special-ctrl-a/e nil))
(setq org-highlight-latex-and-related '(native latex script entities))
(setq org-agenda-files '("~/org/agenda.org"
                         "~/org/notes.org"
                         "~/org/aujourdhui.org"
                         "~/org/math_article_links.org"))
;; (setq org-log-state-notes-insert-after-drawers nil)
(setq org-log-into-drawer t)
(setf org-hide-emphasis-markers t)
(setq org-highest-priority ?A
      org-lowest-priority ?E
      org-default-priority ?B
      ;; org-agenda-deadline-faces '((0.5 . org-warning)
      ;;                             (0.4 . org-upcoming-deadline)
      ;;                             (0.0 . default))
      ;; org-agenda-block-separator ?\—
      org-pretty-entities t)

;; I don't want to toggle. I want to cycle!
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(unless (boundp 'abbrev-prefix-map)
  (define-prefix-command 'abbrev-prefix-map))

;; org tab command!
(map! :map org-mode-map :n [tab] 'org-cycle)

;; org open at point
(after! org
  ;; set the link-open function correctly

  (dolist (scheme '("ftp" "http" "https" "mailto" "news"))
    (org-link-set-parameters scheme
                             :follow
                             (lambda (url &optional arg)
                               (browse-url (concat scheme ":" url) arg))))

  (setf org-link-file-path-type 'noabbrev)

  (map! :map org-mode-map
        :n [?g ?o] 'org-open-at-point-decoded)

  (map! :map org-mode-map
        [?\ù] #'cdlatex-math-symbol
        [?\§] (lambda () (interactive) (insert "\\"))
        :i [?\)] #'end-exit-paren
        :i [backspace] 'durand-delete-pair
        :i [?$] (cmd! (sp-insert-pair "\\("))
        [?\C-c tab] 'durand-forward-link
        [?\C-c S-tab] 'find-previous-link-in-buffer
        [f8] 'org-account-prefix-map
        [?\C-c ?\C-j] 'counsel-org-goto))
;; (define-key org-mode-map [?\ù] abbrev-prefix-map)
;; (define-key org-mode-map [?\§] (lambda () (interactive) (insert "\\")))
;; (add-hook 'org-archive-hook 'org-archive-kill-archive-file)
;; (require 'org-agenda)
;; (use-package! org-super-agenda)
;; (define-key org-mode-map [?\C-c tab] 'durand-forward-link)
;; (define-key org-mode-map [?\C-c \S-tab] 'find-previous-link-in-buffer)
;; (define-key org-mode-map [f8] 'org-account-prefix-map)
;; (define-key org-mode-map [?\C-c ?\C-j] 'counsel-org-goto)

(after! (org-agenda org-super-agenda)
  (map! :map org-agenda-mode-map
        [?\M-n] 'org-super-agenda-next-group
        [?\M-p] 'org-super-agenda-previous-group
        [M-return] 'org-agenda-open-link
        [f8] 'durand-org-account-prefix-map
        [?c] 'durand-agenda
        [?V] 'orgy-view
        [?\)] 'org-agenda-next-block
        [?-] 'org-agenda-previous-block
        [?$] 'org-agenda-go-to-block
        [?j] 'org-agenda-jump-to-item
        [?\C-j] 'org-agenda-goto-date
        [?n] 'org-agenda-next-item
        [?N] 'org-agenda-next-line
        [?p] 'org-agenda-previous-item
        [?P] 'org-agenda-previous-line
        "<backspace>" 'org-agenda-first-block
        [?\d] 'org-agenda-first-block
        "à" 'org-agenda-last-block
        "s-)" 'org-super-agenda-next-group
        "s-(" 'org-super-agenda-previous-group
        "s--" 'org-super-agenda-previous-group
        [?g] #'durand-redo-agenda
        :map org-super-agenda-header-map
        [?g] #'durand-redo-agenda
        [?n] 'org-agenda-next-item
        [?N] 'org-agenda-next-line
        [?p] 'org-agenda-previous-item
        [?P] 'org-agenda-previous-line
        "<backspace>" 'org-agenda-first-block
        "à" 'org-agenda-last-block
        "s-)" 'org-super-agenda-next-group
        "s-(" 'org-super-agenda-previous-group
        "s--" 'org-super-agenda-previous-group
        [f8] 'durand-org-account-prefix-map
        [?\)] 'org-agenda-next-block
        [?-] 'org-agenda-previous-block
        [?j] 'org-agenda-jump-to-item))

;; (advice-remove 'org-edit-special 'org-edit-special-h)
;; (set-face-attribute 'org-block nil :background "gray5" :foreground "DarkOrange1")
;; (set-face-attribute 'bold nil :foreground "OrangeRed1")
;; (set-face-attribute 'org-verbatim nil :background "gray1")
;; (set-face-attribute 'italic nil :foreground "light blue")

(advice-add 'org-agenda-later :around 'durand-agenda-advice)
(advice-add 'org-agenda-goto-date :around 'durand-agenda-advice)
(advice-add 'org-agenda-goto-today :around 'durand-agenda-advice-for-today)

;; (map! :map org-agenda-mode-map [?g] #'durand-redo-agenda)

(map! :map evil-org-mode-map :n (kbd "RET") 'durand-open-link)
(map! :map evil-org-mode-map :n [return] 'durand-open-link)

;; do delete-other-windows
(advice-add 'org-agenda :after '(lambda (&rest params)
                                  "Full frame"
                                  (delete-other-windows)))

(setq org-use-speed-commands t)
(setq org-speed-commands-user
      '(("j" . counsel-org-goto)
        ("P" . org-set-property)
        ("a" . org-toggle-archive-tag)
        ;; ("U" . undo-tree-undo)
        ("k" . kill-current-buffer)
        ("S" . org-schedule)
        ("v" . orgy-view)
        ("§" . durand-org-hydra/body)))

;; link support

;; Record the link types that I know until now.
(defvar durand-link-types '(mu4e-url shr-url button htmlize-link)
  "Link types that I know until now.")

(after! dash
  (put (intern "durand-forward-link")
       'function-documentation
       (concat
        "Forward to "
        (mapconcat #'prin1-to-string
                   (reverse (cdr (reverse durand-link-types)))
                   ", ")
        (format ", or %s" (-last-item durand-link-types))
        " changes.")))

(after! org
  (set-face-attribute 'org-ellipsis nil :foreground nil)

  (setq org-ellipsis " ⤵")

  (add-hook 'org-after-todo-state-change-hook 'durand-org-back-to-repeat)

  (setq org-refile-targets nil))

;; (require 'org-capture)
;; (require 'org-protocol)

;; The original org-protocol-convert handles youtube links wrong
(after! org-capture

  ;; (setq org-capture-templates
  ;;       '(("m" "Account records" entry
  ;;          (file+olp+datetree "~/org/account/account.org")
  ;;          "* %^{ITEM|breakfast|brunch|brunverage|lunch|dinner|beverage|snack|fruit}\n  :PROPERTIES:\n  :cost: %(number-to-string (read-number \"COST:\" 0))\n  :FROM: %(completing-read \"FROM: \" '(\"Cash\" \"etique\"))\n  :RECORD_TIME: %U\n  :END:\n  %(durand-org-complete-capture-account)%?"
  ;;          :jump-to-captured t)
  ;;         ("d" "Record Diaries" entry
  ;;          (file+olp+datetree "~/org/diary.org")
  ;;          "* %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
  ;;          :jump-to-captured t)
  ;;         ("w" "Withdrawal records" entry
  ;;          (file+headline "~/org/wiki.org" "Money Withdrawal")
  ;;          "* WITHDRAW NTD %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil nil)\n"
  ;;          :kill-buffer t)
  ;;         ("l" "Store links" entry
  ;;          (file+headline "~/org/notes.org" "Links")
  ;;          "* TO-THINK %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)\n%a\n" :kill-buffer t)
  ;;         ("g" "GNUS" entry
  ;;          (file "~/org/notes.org")
  ;;          "* TO-THINK %:subject\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n  %:from\n  %:to\n  %a\n  %?"
  ;;          :empty-lines 1
  ;;          :kill-buffer t)
  ;;         ("L" "for storing webpages" entry
  ;;          (function org-determine-link-file)
  ;;          "* PENDING %(org-filter-title) %(org-determine-tag)\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n  %(org-filtered-link)\n  %i\n  %?"
  ;;          :empty-lines 1
  ;;          :kill-buffer t
  ;;          :immediate-finish t)
  ;;         ("t" "TODO" entry
  ;;          (file "~/org/aujourdhui.org")
  ;;          "* TODO %? %^{Date to do:}t\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
  ;;          :kill-buffer t)
  ;;         ("b" "Blog posts" entry
  ;;          (file+headline "~/org/notes.org" "Blog posts")
  ;;          "* %? %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
  ;;         ("a" "Abstractions" entry
  ;;          (file+headline "~/org/wiki.org" "Abstractions")
  ;;          "* ABSTRACT %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n")
  ;;         ("A" "Agenda" entry
  ;;          (file+headline "~/org/agenda.org" "Agenda")
  ;;          "* TODO %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :DURATION: %^{Date: }t\n  :END:\n\n")
  ;;         ("y" "YiFu" entry
  ;;          (file+headline "~/org/wiki.org" "Yi Fu Tips")
  ;;          "* MEMO %^{word}\n  :PROPERTIES:\n  :STORY: %\\2\n  :MEANING: %\\3\n  :END:\n** Yi Fu story\n   %^{story}\n** Meaning\n   %^{meaning}"
  ;;          :kill-buffer t
  ;;          :immediate-finish t)
  ;;         ("c" "Chansons" entry
  ;;          (file+headline "~/org/wiki.org" "Liste de Chansons")
  ;;          "* MEMO %^{title}\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :LINK: [[%^{link}][%^{description}]]\n  :END:\n  %?"
  ;;          :jump-to-captured t)
  ;;         ("f" "français" entry
  ;;          (file+headline "~/org/français/français.org" "Liste de mots français")
  ;;          "* MEMO %^{mot} :drill:\n  :PROPERTIES:\n  :DRILL_CARD_TYPE: français\n  :RECORD_TIME: %U\n  :MEANING: %^{ce qu'il veut dire}\n  :END:\n\n  MEANING: %\\2\n%?"
  ;;          :jump-to-captured t)))

  (setf org-capture-templates-contexts
        '(("g" ((in-mode . "gnus-summary-mode")
                (in-mode . "gnus-article-mode")))))

  (set-face-attribute 'org-document-title nil :font "Futura-30")

  (add-hook 'org-capture-mode-hook (lambda ()
                                     "Activate account minor mode if in capturing accounts"
                                     (when (s-suffix? "account.org" (buffer-name))
                                       (account-mode))))
  (add-hook 'org-capture-after-finalize-hook 'durand-capture-update-account)
  (add-hook 'org-capture-prepare-finalize-hook 'durand-capture-set-window-conf)
  (advice-add 'org-capture-goto-last-stored :after 'durand-capture-reposition-windows)
  )

(defvar durand-account-report-period-str "LAST DAY"
  "The string to show in report buffer.
This should be setted by the PERIOD-FUNC argument.")

;;; completion in account capture

;;;###autoload
(defvar durand-frequent-shops nil
  "Shops I frequently visit.")

(setq durand-frequent-shops (list
                             "新永泉"
                             "Kebuke"
                             "師園"
                             "十杯"
                             ;; "即食樂"
                             ;; "來吃麵"
                             ;; "鍋兵鍋將"
                             ;; "羅胖子"
                             ;; "牛魔王"
                             ;; "光華夜市鱔魚意麵"
                             ;; "光華夜市酵素臭豆腐"
                             ))

;;;###autoload
(defvar durand-complete-shop-history nil
  "History variable for completing shops.")

;; account report pop up rule
(set-popup-rule! "\\*ACCOUNT REPORT\\*"
  :side 'bottom
  :height 0.5
  :quit t
  :select t)

(map! :map account-report-mode-map
      [?<] #'durand-account-first-entry
      [?>] #'durand-account-last-entry
      [?d] #'durand-view-last-day
      [?w] #'durand-view-last-week
      [?m] #'durand-view-last-month
      [?y] #'durand-view-last-year
      [?c] #'durand-view-last-custom
      [?s] #'durand-view-include
      [?e] #'durand-view-exclude
      [?r] #'durand-view-report-mode
      [?j] #'durand-view-go-to-account-day
      [?n] #'durand-view-go-to-next-day
      [?p] #'durand-view-go-to-previous-day
      [?v] #'durand-view-entry
      [?x] #'kill-current-buffer
      [?q] #'kill-current-buffer)

;;;###autoload
(setq account-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode account-mode "For completing in capturing accounts"
  nil
  "ACCOUNT"
  account-mode-map)

;; (add-hook 'org-capture-mode-hook (lambda ()
;;                                    "Activate account minor mode if in capturing accounts"
;;                                    (when (s-suffix? "account.org" (buffer-name))
;;                                      (account-mode))))

;; (map! :map account-mode-map
;;       [tab] 'org-smart-complete-item-or-shop-or-jump-to-next-item)

(setq org-agenda-use-time-grid nil)
                                        ; The time grid in agenda view is a little annoying
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-start-with-log-mode nil)
(setq org-global-properties '((Effort_ALL . "0 0:10 0:20 0:30 0:40 0:50 1:00 1:30 2:00")))
(setq org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM %PRIORITY")

(after! org-agenda
  (setf org-agenda-start-day "+0d"
        org-agenda-dim-blocked-tasks nil))

(after! calendar
  (setf diary-file (expand-file-name "diary" org-directory)))

(use-package! org-super-agenda
  :after-call durand-agenda
  :init
  (setf org-agenda-custom-commands
        '(("o" "Custom"
           ((agenda ""
                    ((org-super-agenda-groups
                      '((:name "Progress today"
                         :log t
                         :order -1)
                        (:name "Morning Tasks"
                         :log t
                         :tag "morning"
                         :order 1)
                        (:name "Afternoon Taks"
                         :log t
                         :tag "afternoon"
                         :order 2)
                        (:name "Night Taks"
                         :log t
                         :tag "night"
                         :order 3)
                        (:name "Deadlines" :deadline t)
                        (:name "Health"
                         :tag "santé"
                         :log t
                         :order 5)
                        (:name "MATH"
                         :tag "math"
                         :order -1)
                        (:name "Très Important"
                         :priority "A"
                         :order -1)
                        (:name "Scheduled"
                         :and (:scheduled t :not (:priority "A"))
                         :order 5
                         :log t)))
                     (org-agenda-span 'day)
                     (org-agenda-sorting-strategy '(priority-down time-up))))
            ;; (tags "plan"
            ;;       ((org-agenda-files '("~/org/plan.org"))
            ;;        (org-super-agenda-groups
            ;;         '((:name "Début" :todo "DÉBUT")
            ;;           (:name "Essaiyer" :todo "ESSAIYER")
            ;;           (:name "Progresser" :todo "PROGRESSER")
            ;;           (:name "Complété" :todo "COMPLÉTÉ")))
            ;;        (org-agenda-overriding-header "PLAN")))
            ;; NOTE: A reading plan
            (tags "a_voir+matin|a_voir+après_midi|a_voir+nuit"
                  ((org-agenda-files '("~/org/notes.org"
                                       "~/org/math_article_links.org"))
                   (org-agenda-overriding-header "À Lire")
                   (org-super-agenda-groups
                    '((:name "Le Matin" :tag "matin")
                      (:name "L'après-midi" :tag "après_midi")
                      (:name "La Nuit" :tag "nuit")))))
            (todo "TO-THINK"
                  ((org-super-agenda-groups
                    '((:name "À Voir" :tag "a_voir")
                      (:name "Mathématiques" :tag "math")
                      (:name "TeX" :tag "tex")
                      (:name "Question" :tag "question")))
                   (org-agenda-overriding-header "TO-THINK"))))
           ((org-agenda-block-separator nil)))
          ("n" "test"
           ((durand-agenda-command-test)))
          ("m" "sections with time"
           ((durand-agenda-command-sections-time)))))
  :config
  (advice-add 'org-agenda-log-mode :after 'evil-emacs-state)
  (add-hook 'org-agenda-mode-hook #'org-agenda-first-block)
  (org-super-agenda-mode))

;; (map! :leader :n "oaa" #'durand-agenda)


(define-prefix-command 'durand-org-account-prefix-map)
(define-key durand-org-account-prefix-map [?a] 'durand-org-agenda-append-text)
(define-key durand-org-account-prefix-map [?v] 'durand-org-agenda-goto-view-note)

;; I like auto-fill-mode a lot.

(add-hook 'org-mode-hook 'durand-org-activate-auto-fill t)

;;; my own preparations in org-mode
(add-hook 'org-mode-hook 'durand-org-preparation t)

(after! org-capture
  (add-hook 'org-capture-mode-hook (lambda ()
                                     "Activate fill-column in org capture"
                                     (interactive)
                                     (setq-local fill-column durand-org-fill-column)
                                     (auto-fill-mode 1))))
(add-hook 'org-log-buffer-setup-hook
          (lambda ()
            "Activate fill-column in org capture"
            (interactive)
            (setq-local fill-column durand-org-fill-column)
            (auto-fill-mode 1)))

;; major mode for durand-org-view-notes

;;;###autoload
(define-derived-mode durand-org-notes-mode org-mode "Durand View Notes"
  "A mode for viewing notes.")

(map! :map durand-org-notes-mode-map
      :n [?q] 'quit-window)

;; pop up rule for durand-org-view-notes
(set-popup-rule! "\\*durand-org-view-notes\\*"
  :size #'+popup-shrink-to-fit
  :side 'bottom
  :quit t
  :select t
  :modeline nil)

(setq temp-buffer-max-height
      (/ (frame-height) 3))

(after! org-agenda
  (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1))))

;;; novel addresses
;;;###autoload
(defvar durand-novel-addresses-regexp '("uukanshu"
                                        "ptwxz"
                                        "piaotian"
                                        "101novel"
                                        "booktxt")
  "Regexp for matching a novel website.")

;; org clock
(setq org-clock-mode-line-total 'current)
;; (setq org-clock-persist t)
;; (org-clock-persistence-insinuate)

;; adjust org-goto styles
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
(setq counsel-outline-face-style nil)
(setq counsel-org-goto-separator " ➜ ")

(define-prefix-command 'org-account-prefix-map)
(define-key org-account-prefix-map [?n] 'org-new-account)
(define-key org-account-prefix-map [?u] 'org-update-account)
(define-key org-account-prefix-map [?c] 'org-calc-account)
(define-key org-account-prefix-map [?r] 'org-run-src-block)
(define-key org-account-prefix-map [?i] 'org-set-item-price-note)
(define-key org-account-prefix-map [?m] 'org-modify-account)
(define-key org-account-prefix-map [?d] 'org-delete-account)
(define-key org-account-prefix-map [?g] 'org-account-go-to-day)
(define-key org-account-prefix-map [?G] 'org-account-go-to-last-day)
(define-key org-account-prefix-map [?q] 'org-clear-buffers)
(define-key org-account-prefix-map [tab] 'durand-forward-link)
(define-key org-account-prefix-map [S-tab] 'find-previous-link-in-buffer)
(define-key org-account-prefix-map [?v] #'durand-org-view-notes)
(define-key org-account-prefix-map [?a] #'durand-show-account-report)

;; (after! org-fancy-priorities
;;   (setf org-fancy-priorities-list
;;         (list "I"
;;               "II"
;;               "III"
;;               "IV"
;;               "V")
;;         ;; (append (-take 3 org-fancy-priorities-list) (list "■" "■" ))
;;         org-priority-faces
;;         (append (-take 3 org-priority-faces)
;;                 (list '(?D :foreground "lightskyblue1")
;;                       '(?E :foreground "DeepSkyBlue1")))))

;; set up org-pdfview

;; (use-package! org-pdfview
;;   :after-call org-open-articles)

;;; subtree map
(defvar durand-org-subtree-map (make-sparse-keymap)
  "A keymap for subtree operations in org-mode buffers.")

(after! org
  (define-key durand-org-subtree-map [?a] 'org-archive-subtree)
  (define-key durand-org-subtree-map [?c] 'org-copy-subtree)
  (define-key durand-org-subtree-map [?k] 'org-cut-subtree)
  (define-key durand-org-subtree-map [?p] 'org-paste-subtree)
  (map! :map org-mode-map
        :localleader
        :n [?a ?g] 'durand-org-goto-archive
        :n [?s] durand-org-subtree-map
        :n [?S] 'org-schedule))

;;; use xelatex as engine

(after! ox-latex
  (when (boundp 'org-latex-pdf-process)
    (setf org-latex-pdf-process
          (mapcar
           (lambda (s)
             (replace-regexp-in-string
              "%latex" "xelatex" s))
           org-latex-pdf-process))))

;;; org-noter

(when (featurep! :lang org +noter)
  (use-package! org-noter
    :after-call org-noter
    :config
    (setf org-noter-notes-search-path (list "/Users/durand/org/mes-notes/")
          org-noter-always-create-frame nil
          org-noter-kill-frame-at-session-end nil
          org-noter-notes-window-location 'vertical-split)
    (require 'pdf-tools)
    (map! :map pdf-view-mode-map
          :n [?g ?'] 'durand-org-noter-edit-document)
    (map! :map org-mode-map
          :n [?g ?'] 'durand-org-noter-go-to-doc)))

;;; org-roam

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph-show
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :config
  ;; set it explicitly to org-directory
  (setf org-roam-directory (expand-file-name "roam" org-directory)
        org-roam-dailies-capture-templates
        '(("d" "daily"
           plain #'org-roam-capture--get-point ""
           :immediate-finish t
           :file-name "journal/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>")))
  ;; use separators
  (setf org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point "%?"
           :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%S\")-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  ;; ignore certain files
  (setf org-roam-graph-exclude-matcher '("account" "ltximg" "screen-shots")
        ;; Don't open org-roam when I don't want to!
        +org-roam-open-buffer-on-find-file nil
        ;; use safari for viewing svg files
        org-roam-graph-viewer 'durand-view-svg
        org-roam-graph-executable (executable-find "neato")
        org-roam-graph-extra-config '(("overlap" . "false")))
  ;; Org mode tags matter
  (setf org-roam-tag-sources '(prop vanilla)))

(use-package! org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (org-roam-server-mode))

;; org-ql

;; (use-package! org-ql
;;   :after-call durand-agenda
;;   :after org-agenda
;;   (setf (alist-get "n" org-agenda-custom-commands nil nil #'string=)
;;         '("test"
;;           ((org-ql-block '(and (ts-a :to 1 :from -1)
;;                                (todo))
;;                          ((org-ql-block-header "Testing")))))))
