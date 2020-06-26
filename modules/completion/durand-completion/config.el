;;; completion/durand-completion/config.el -*- lexical-binding: t; -*-

(use-package! icomplete
  :hook (doom-first-input . icomplete-mode)
  :config

  (fido-mode -1)

  (setf icomplete-show-matches-on-no-input t
        icomplete-hide-common-prefix nil
        icomplete-separator (propertize " ┆ " 'face 'shadow)
        icomplete-max-delay-chars 1
        icomplete-delay-completions-threshold 100
        icomplete-compute-delay 0.02
        icomplete-with-completion-tables t
        icomplete-in-buffer t
        icomplete-prospects-height 1
        icomplete-tidy-shadowed-file-names t)

  ;; NOTE: This is already covered by `icomplete-vertical-mode'.
  ;; (add-hook 'icomplete-minibuffer-setup-hook 'prot/icomplete-minibuffer-truncate)

  :bind (:map minibuffer-local-map
         ("<backspace>" . durand-icomplete-backward-updir)
         ("<delete>" . durand-icomplete-backward-updir)
         ;; ("<down>" . embark-forward-completions)
         ;; ("<right>" . embark-forward-completions)
         ;; ("C-n" . embark-forward-completions)
         ;; ("<up>" . embark-backward-completions)
         ;; ("<left>" . embark-backward-completions)
         ;; ("C-p" . embark-backward-completions)
         ;; ("<return>" . exit-minibuffer)
         ;; ("C-j" . exit-minibuffer)
         ;; :map minibuffer-local-completion-map
         ;; ("<return>" . minibuffer-complete-and-exit)
         ;; ("C-j" . exit-minibuffer)
         :map read-expression-map
         ("C-j" . exit-minibuffer)
         :map icomplete-minibuffer-map
         ;; ("<return>" . minibuffer-complete-and-exit)
         ("C-j" . exit-minibuffer)
         ("C-n" . icomplete-forward-completions)
         ("C-p" . icomplete-backward-completions)
         ("<C-backspace>" . durand-crm-backward-delete-to-separator)
         ("C-w" . durand-crm-backward-delete-to-separator)
         ("s-f" . durand-crm-forward-separator)
         ("s-b" . durand-crm-backward-separator)
         ("<tab>" . minibuffer-force-complete)
         ("<return>" . icomplete-force-complete-and-exit)))

(use-package! icomplete-vertical
  :after icomplete
  :config
  (define-key icomplete-minibuffer-map [?\C-v] 'icomplete-vertical-toggle)
  ;; add advices so that some functions show no candidates without input
  ;; (advice-add 'helpful-callable :before 'prot/icomplete-empty-input-no-list)
  ;; (advice-add 'helpful-variable :before 'prot/icomplete-empty-input-no-list)
  ;; (advice-add 'execute-extended-command :before 'prot/icomplete-empty-input-no-list)
  )

(use-package! embark
  :after (icomplete orderless)
  :config
  (define-key minibuffer-local-completion-map [?\M-o] 'embark-act)
  (define-key minibuffer-local-completion-map [?\C-o] 'embark-export)
  ;; Make C-h {f,v,o} use vertical layout with icomplete-mode.
  ;; (advice-add 'describe-function :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'describe-variable :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'describe-symbol :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'helpful-callable :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'helpful-variable :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'helpful-symbol :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'doom/help-package-config :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'doom/goto-private-packages-file :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'doom/help-package-homepage :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'doom/help-packages :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'find-file :around 'durand-icomplete-vertical-around)
  ;; (advice-add 'doom/find-file-in-private-config :around 'durand-icomplete-vertical-around)
  ;; no multiple embark occur buffers!
  ;; (add-hook 'minibuffer-exit-hook 'prot/embark-live-occur-single-buffer)
  (advice-add 'embark-live-occur :after 'durand-embark-switch-to-minibuffer-a))

(use-package! minibuffer
  :config
  ;; (define-key minibuffer-local-completion-map [return] 'minibuffer-force-complete-and-exit)
  ;; (define-key minibuffer-local-completion-map [?\C-j] 'exit-minibuffer)
  (define-key minibuffer-local-completion-map [32] nil)

  (setf enable-recursive-minibuffers t))

(use-package! orderless
  :after icomplete
  :config

  ;; NOTE: flex style is added after emacs 27.
  ;; (setf completion-styles '(flex orderless partial-completion))
  (setq completion-styles
        '(flex partial-completion))
  (setf orderless-matching-styles
        '(;; orderless-flex
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-prefixes
          orderless-literal))

  (setf orderless-style-dispatchers
        '(durand-company-style-dispatcher))

  (setf orderless-component-separator "[ &]+")

  (setq-default indent-tabs-mode nil)

  (setf tab-always-indent 'complete
        completion-in-region-function #'contrib/completing-read-in-region))

;;; From Protesilaos dotemacs
(use-package! dabbrev
  :after (minibuffer icomplete icomplete-vertical) ; read those as well
  :config

  (setf dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
        dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|="
        dabbrev-backward-only nil
        dabbrev-case-distinction nil
        dabbrev-case-fold-search t
        dabbrev-case-replace nil
        dabbrev-check-other-buffers t
        dabbrev-eliminate-newlines nil
        dabbrev-upcase-means-case-search t)

  :bind (("M-/" . dabbrev-expand)
         ("C-M-/" . dabbrev-completion)
         ("s-/" . dabbrev-completion)))
