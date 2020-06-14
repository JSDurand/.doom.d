;;; completion/durand-completion/config.el -*- lexical-binding: t; -*-

(use-package! icomplete
  :hook (doom-first-input . icomplete-mode)
  :config

  (setf icomplete-show-matches-on-no-input t
        icomplete-hide-common-prefix nil
        icomplete-separator (propertize " ┆ " 'face 'shadow)
        icomplete-max-delay-chars 1
        icomplete-delay-completions-threshold 100
        icomplete-compute-delay 0.02
        icomplete-with-completion-tables t
        icomplete-in-buffer t
        icomplete-prospects-height 1
        icomplete-tidy-shadowed-file-names nil)

  (add-hook 'icomplete-minibuffer-setup-hook 'prot/icomplete-minibuffer-truncate)

  :bind (:map icomplete-minibuffer-map

         ("<tab>" . 'minibuffer-force-complete)
         ("<down>" . 'icomplete-forward-completions)
         ("<right>" . 'icomplete-forward-completions)
         ("C-n" . 'icomplete-forward-completions)
         ("<up>" . 'icomplete-backward-completions)
         ("<left>" . 'icomplete-backward-completions)
         ("C-p" . 'icomplete-backward-completions)
         ("<return>" . 'icomplete-force-complete-and-exit)
         ("C-j" . 'exit-minibuffer)))

(use-package! icomplete-vertical
  :after icomplete
  :bind (:map icomplete-minibuffer-map
         ("C-v" . 'icomplete-vertical-toggle))
  :config
  ;; add advices so that some functions show no candidates without input
  (advice-add 'helpful-callable :before 'prot/icomplete-empty-input-no-list)
  (advice-add 'helpful-variable :before 'prot/icomplete-empty-input-no-list)
  (advice-add 'execute-extended-command :before 'prot/icomplete-empty-input-no-list))

(use-package! embark
  :after (icomplete orderless)
  :bind (:map minibuffer-local-completion-map
         ("M-o" . 'embark-act)
         ("C-o" . 'embark-export))
  :config
  ;; Make C-h {f,v,o} use vertical layout with icomplete-mode.
  (advice-add 'describe-function :before 'durand-icomplete-vertical)
  (advice-add 'describe-variable :before 'durand-icomplete-vertical)
  (advice-add 'describe-symbol   :before 'durand-icomplete-vertical)
  (advice-add 'helpful-callable  :before 'durand-icomplete-vertical)
  (advice-add 'helpful-variable  :before 'durand-icomplete-vertical)
  (advice-add 'helpful-symbol    :before 'durand-icomplete-vertical)
  ;; no multiple embark occur buffers!
  (add-hook 'minibuffer-exit-hook 'prot/embark-live-occur-single-buffer))

(use-package! minibuffer
  :bind (:map minibuffer-local-completion-map
         ("<return>" . 'minibuffer-force-complete-and-exit)
         ("C-j" . 'exit-minibuffer)
         ("SPC" . nil))

  :config

  (setf enable-recursive-minibuffers t))

(use-package! orderless
  :after icomplete
  :config

  (setf completion-styles '(orderless partial-completion))

  (setf orderless-matching-styles
        '(orderless-flex
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-prefixes
          orderless-literal))

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
