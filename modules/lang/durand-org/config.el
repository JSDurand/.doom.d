;; use amsfont in latex preview
(setf org-latex-packages-alist '(("" "amsfonts" t)))
;; Necessary since org-mode 9.2
(ignore-errors (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("g" . "src durand-greek"))
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "START(s)" "WORKING(w)" "HARD-WORKING(h)" "ALMOST(a)" "|" "DONE(d)")
          (sequence "TO-THINK(c)" "PENDING(p)" "HARD(r)" "IMPOSSIBLE(i)" "|" "SOLVED(v)"))
        org-tags-column -110
        org-special-ctrl-a/e nil))
(setq org-agenda-files '("~/org/agenda.org" "~/org/notes.org" "~/org/aujourdhui.org"))
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

(unless (boundp 'abbrev-prefix-map)
  (define-prefix-command 'abbrev-prefix-map))

;; org tab command!
;; (map! :map org-mode-map :n [tab] 'org-cycle)

(map! :map org-mode-map
      [?\ù] abbrev-prefix-map
      [?\§] (lambda () (interactive) (insert "\\"))
      [?\C-c tab] 'durand-forward-link
      [?\C-c \S-tab] 'find-previous-link-in-buffer
      [f8] 'org-account-prefix-map
      [?\C-c ?\C-j] 'counsel-org-goto)
;; (define-key org-mode-map [?\ù] abbrev-prefix-map)
;; (define-key org-mode-map [?\§] (lambda () (interactive) (insert "\\")))
;; (add-hook 'org-archive-hook 'org-archive-kill-archive-file)
(require 'org-agenda)
(use-package! org-super-agenda)
;; (define-key org-mode-map [?\C-c tab] 'durand-forward-link)
;; (define-key org-mode-map [?\C-c \S-tab] 'find-previous-link-in-buffer)
;; (define-key org-mode-map [f8] 'org-account-prefix-map)
;; (define-key org-mode-map [?\C-c ?\C-j] 'counsel-org-goto)

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
      :map org-super-agenda-header-map
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
      [?j] 'org-agenda-jump-to-item)

(advice-add 'org-edit-special :after '(lambda (&optional orig-fun)
                                        "Make it full frame"
                                        (delete-other-windows)))
;; (set-face-attribute 'org-block nil :background "gray5" :foreground "DarkOrange1")
;; (set-face-attribute 'bold nil :foreground "OrangeRed1")
;; (set-face-attribute 'org-verbatim nil :background "gray1")
;; (set-face-attribute 'italic nil :foreground "light blue")

(advice-add 'org-agenda-later :around 'durand-agenda-advice)
(advice-add 'org-agenda-goto-today :around 'durand-agenda-advice-for-today)

(map! :map org-agenda-mode-map [?g] #'durand-redo-agenda)

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
        ("U" . undo-tree-undo)
        ("k" . kill-current-buffer)
        ("S" . org-schedule)
        ("v" . orgy-view)
        ("§" . durand-org-hydra/body)))

;; link support

;; Record the link types that I know until now.
(defvar durand-link-types '(mu4e-url shr-url button htmlize-link)
  "Link types that I know until now.")

(put (intern "durand-forward-link")
     'function-documentation
     (concat
      "Forward to "
      (mapconcat #'prin1-to-string
                 (reverse (cdr (reverse durand-link-types)))
                 ", ")
      (format ", or %s" (-last #'identity durand-link-types))
      " changes."))

(set-face-attribute 'org-ellipsis nil :foreground nil)

(setq org-ellipsis " ⤵")

(add-hook 'org-after-todo-state-change-hook 'durand-org-back-to-repeat)

(setq org-refile-targets nil)

(require 'org-capture)
(require 'org-protocol)

;; The original org-protocol-convert handles youtube links wrong
(setq org-capture-templates
      '(("m" "Account records" entry
	 (file+datetree "~/org/account/account.org")
	 "* %^{ITEM|breakfast|brunch|brunverage|lunch|dinner|beverage|snack|fruit}\n  :PROPERTIES:\n  :cost: %^{COST|0}\n  :FROM: %^{FROM|Cash}\n  :RECORD_TIME: %U\n  :END:\n  %?\n\n  - "
	 :jump-to-captured t)
	("d" "Record Diaries" entry
	 (file+datetree "~/org/diary.org")
	 "* %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
	 :jump-to-captured t)
	("w" "Withdrawal records" entry
	 (file+headline "~/org/wiki.org" "Money Withdrawal")
	 "* WITHDRAW NTD %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil nil)\n"
	 :kill-buffer t)
	("l" "Store links" entry
	 (file+headline "~/org/notes.org" "Links")
	 "* TO-THINK %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)\n%a\n" :kill-buffer t)
        ("L" "for storing webpages" entry
         (file+headline "~/org/notes.org" "Links")
         "* PENDING %(org-filter-title) %(org-determine-tag)\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n  %(org-filtered-link)\n  %i\n  %?"
         :empty-lines 1
         :kill-buffer t
         :immediate-finish t)
	("t" "TODO" entry
	 (file "~/org/aujourdhui.org")
	 "* TODO %? %^{Date to do:}t\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
	 :kill-buffer t)
	("b" "Blog posts" entry
	 (file+headline "~/org/notes.org" "Blog posts")
	 "* %? %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
	("a" "Abstractions" entry
	 (file+headline "~/org/wiki.org" "Abstractions")
	 "* ABSTRACT %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n")
	("A" "Agenda" entry
	 (file+headline "~/org/agenda.org" "Agenda")
	 "* TODO %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :DURATION: %^{Date: }t\n  :END:\n\n")
	("y" "YiFu" entry
	 (file+headline "~/org/wiki.org" "Yi Fu Tips")
	 "* MEMO %^{word}\n  :PROPERTIES:\n  :STORY: %\\2\n  :MEANING: %\\3\n  :END:\n** Yi Fu story\n   %^{story}\n** Meaning\n   %^{meaning}"
	 :kill-buffer t
	 :immediate-finish t)
        ("c" "Chansons" entry
	 (file+headline "~/org/wiki.org" "Liste de Chansons")
	 "* MEMO %^{title}\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :LINK: %A\n  :END:\n  %?"
	 :jump-to-captured t)
        ("f" "français" entry
	 (file+headline "~/org/français/français.org" "Liste de mots français")
	 "* MEMO %^{mot} :drill:\n  :PROPERTIES:\n  :DRILL_CARD_TYPE: français\n  :RECORD_TIME: %U\n  :MEANING: %^{ce qu'il veut dire}\n  :END:\n\n  MEANING: %\\2\n%?"
	 :jump-to-captured t)))

(add-hook 'org-capture-after-finalize-hook 'durand-capture-update-account)

(defvar durand-account-report-period-str "LAST DAY"
  "The string to show in report buffer.
This should be setted by the PERIOD-FUNC argument.")

(map! :map account-report-mode-map
      [?d] #'durand-view-last-day
      [?w] #'durnad-view-last-week
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
      [?x] #'durand-kill-buffer)

;;;###autoload
(setq account-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode account-mode "For completing in capturing accounts"
  nil
  "ACCOUNT"
  account-mode-map)

(add-hook 'org-capture-mode-hook (lambda ()
                                   "Activate account minor mode if in capturing accounts"
                                   (when (s-suffix? "account.org" (buffer-name))
                                     (account-mode))))

;; (map! :map account-mode-map
;;       [tab] 'org-smart-complete-item-or-shop-or-jump-to-next-item)

(setq org-agenda-use-time-grid nil)
; The time grid in agenda view is a little annoying
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-start-with-log-mode nil)
(setq org-global-properties '((Effort_ALL . "0 0:10 0:20 0:30 0:40 0:50 1:00 1:30 2:00")))
(setq org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM %PRIORITY")

(after! org-agenda
  (setf org-agenda-start-day "+0d"))

(use-package! org-super-agenda
  :after org-agenda
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
            (tags "plan"
                  ((org-agenda-files '("~/org/plan.org"))
                   (org-super-agenda-groups
                    '((:name "Début" :todo "DÉBUT")
                      (:name "Essaiyer" :todo "ESSAIYER")
                      (:name "Progresser" :todo "PROGRESSER")
                      (:name "Complété" :todo "COMPLÉTÉ")))
                   (org-agenda-overriding-header "PLAN")))
            (todo "TO-THINK"
                  ((org-super-agenda-groups
                    '((:name "À Voir" :tag "a_voir")
                      (:name "Mathématiques" :tag "math")
                      (:name "TeX" :tag "tex")
                      (:name "Question" :tag "question")))
                   (org-agenda-overriding-header "TO-THINK"))))
           ((org-agenda-block-separator nil)))))
  :config
  (org-super-agenda-mode))

(set-evil-initial-state!
  '(org-agenda-mode)
  'emacs)

(map! :leader :n "oaa" #'durand-agenda)

(add-hook 'org-agenda-mode-hook #'org-agenda-first-block)

(define-prefix-command 'durand-org-account-prefix-map)
(define-key durand-org-account-prefix-map [?a] 'durand-org-agenda-append-text)
(define-key durand-org-account-prefix-map [?v] 'durand-org-agenda-goto-view-note)

;; I like auto-fill-mode a lot.

(add-hook 'org-mode-hook (lambda ()
                           "Activate `auto-fill-mode'."
                           (interactive)
                           (setq-local fill-column 90)
                           (auto-fill-mode 1)) t)

(add-hook 'org-capture-mode-hook (lambda ()
				                           "Activate fill-column in org capture"
				                           (interactive)
				                           (setq-local fill-column 90)
				                           (auto-fill-mode 1)))
(add-hook 'org-log-buffer-setup-hook
	  (lambda ()
	    "Activate fill-column in org capture"
	    (interactive)
	    (setq-local fill-column 90)
	    (auto-fill-mode 1)))

;; pop up rule for durand-org-view-notes
(set-popup-rule! "\\*durand-org-view-notes\\*"
  :size #'+popup-shrink-to-fit
  :side 'bottom
  :quit t
  :select t
  :modeline nil)

(setq temp-buffer-max-height
      (/ (frame-height) 3))

(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

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

(after! org-fancy-priorities
  (setf org-fancy-priorities-list
        (list "I"
              "II"
              "III"
              "IV"
              "V")
        ;; (append (-take 3 org-fancy-priorities-list) (list "■" "■" ))
        org-priority-faces
        (append (-take 3 org-priority-faces)
                (list '(?D :foreground "lightskyblue1")
                      '(?E :foreground "DeepSkyBlue1")))))
