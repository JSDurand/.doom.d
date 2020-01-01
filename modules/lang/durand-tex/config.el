;;; .doom.d/modules/lang/durand-tex/config.el -*- lexical-binding: t; -*-

;; (require 'tex)
;; (require 'ivy)
;; (require 'latex)
;; (require 'pdf-view)

;;;###autoload
(defvar tex-root-file-name nil
  "Root file for tex")

;;;###autoload
(defvar working-name nil
  "working file name for tex")

;;;###autoload
(defvar tex-changed nil
  "Non-nil if just used `tex'")

;;;###autoload
(defvar tex-output-bn nil
  "The name of the buffer used for displaying the output of tex process")

(make-variable-buffer-local 'tex-output-bn)
(make-variable-buffer-local 'tex-changed)

(unless (boundp 'abbrev-prefix-map)
  (defvar abbrev-prefix-map (make-sparse-keymap)
    "Keymap for expanding abbreviations in TeX and LaTeX."))

;;;###autoload
(defvar tex-heading-list nil
  "The list of headings used in tex files")

;;;###autoload
(setq tex-heading-list '("heading"
                         "imp"
                         "cep"
                         "thm"
                         "sec"
                         "secc"
                         "chap"
                         "tit"
                         "lem"))

(use-package! olivetti
  :after tex
  :config
  (setq-default olivetti-body-width 90))

;; reset the prefix
(setq! LaTeX-math-abbrev-prefix "ù")

;; add mathscr
(after! latex
  (setf (alist-get ?\C-c LaTeX-font-list)
        '("\\textsc{" "}" "\\mathscr{" "}")))

;; (add-hook 'TeX-mode-hook 'olivetti-mode)
;; oft macros are close to each other, this makes it easy to distinguish them.
;;;###autoload
(defun change-syntax-entry ()
  "oft macros are close to each other, this makes it easy to distinguish them."
  (modify-syntax-entry ?\\ "_" (syntax-table)))

(add-hook! LaTeX-mode '(change-syntax-entry olivetti-mode outline-minor-mode))
(add-hook! TeX-mode '(change-syntax-entry olivetti-mode outline-minor-mode))
;; (modify-syntax-entry ?\\ "_" TeX-mode-syntax-table)
;; (modify-syntax-entry ?\\ "_" LaTeX-mode-syntax-table)
;;;###autoload
(defface tex-big-face '((t (:height 1.5 :foreground "orange1")))
  "Highlight special headings in a big font!")

;; The special fontification should be done by `font-lock-add-keywords' instead.
(font-lock-add-keywords 'TeX-mode `((,(concat
                                             "^\\\\\\(?:"
                                             (mapconcat #'identity
                                                        tex-heading-list
                                                        "\\|")
                                             "\\) \\([^\n]+\\)$")
                                           1 'tex-big-face t))
                        'append)
(map! :map TeX-mode-map
      :i [?\§] '(lambda () "remap to type escape key." (interactive) (insert "\\"))
      :n [f9] #'tex
      :n [f11] #'tex-display-or-kill-temp-buffer
      :n [f7] #'tex-set-up-root
      :n [f8] #'tex-set-up-working
      :i [?\)] #'end-exit-paren
      :i [?ç] 'open-back-paren
      :i [?\(] 'open-paren
      :i [backspace] 'durand-delete-pair
      :i [?\{] 'open-curly
      :i [?\[] 'open-bracket
      :prefix [?\C-c]
      :vni [?d] 'insert-def
      :vni [?o] 'one-def
      :vni [?t] 'two-def
      :i [?\C-c] 'durand-general-save-buffer
      :i [?r] 'read-tex-complete)

(map! :map TeX-mode-map
      :i [?\M-'] 'abbrev-prefix-mark
      ;; :i [?ù] abbrev-prefix-map
      :i [tab] 'company-complete-common-or-cycle)

(map! :map LaTeX-mode-map
      :i [?\§] '(lambda () "remap to type escape key." (interactive) (insert "\\"))
      :i [f9] #'tex
      :i [f11] #'tex-display-or-kill-temp-buffer
      :i [f7] #'tex-set-up-root
      :i [f8] #'tex-set-up-working
      :i [?\)] #'end-exit-paren
      :i [?ç] 'open-back-paren
      :i [?\(] 'open-paren
      :i [backspace] 'durand-delete-pair
      :i [?\{] 'open-curly
      :i [?\[] 'open-bracket
      :prefix [?\C-c]
      :vni [?d] 'insert-def
      :vni [?o] 'one-def
      :vni [?t] 'two-def
      :i [?\C-c] 'durand-general-save-buffer
      :i [?r] 'read-tex-complete)
(map! :map LaTeX-mode-map
      :i [?\M-'] 'abbrev-prefix-mark
      ;; :i [?ù] abbrev-prefix-map
      :i [tab] 'company-complete-common-or-cycle)

;; (define-key abbrev-prefix-map "a" (lambda () (interactive) (insert "\\alpha")))
;; (define-key abbrev-prefix-map "$" (lambda () (interactive) (insert "\\(\\)") (backward-char 2)))
;; (define-key abbrev-prefix-map "*" (lambda () (interactive) (insert "\\[\\]") (backward-char 2)))
;; (define-key abbrev-prefix-map "-" (lambda () (interactive) (insert "\\setminus")))
;; (define-key abbrev-prefix-map ")" (lambda () (interactive) (insert "\\supset")))
;; (define-key abbrev-prefix-map "(" (lambda () (interactive) (insert "\\subset")))
;; (define-key abbrev-prefix-map "]" (lambda () (interactive) (insert "\\supseteq")))
;; (define-key abbrev-prefix-map "[" (lambda () (interactive) (insert "\\subseteq")))
;; (define-key abbrev-prefix-map "{" (lambda () (interactive) (insert "\\left\\{\\right\\}") (backward-char 8)))
;; (define-key abbrev-prefix-map "I" (lambda () (interactive) (insert "\\infty")))
;; (define-key abbrev-prefix-map "i" (lambda () (interactive) (insert "\\in")))
;; (define-key abbrev-prefix-map "A" (lambda () (interactive) (insert "\\forall")))
;; (define-key abbrev-prefix-map "." (lambda () (interactive) (insert "\\cdot")))
;; (define-key abbrev-prefix-map "v." (lambda () (interactive) (insert "\\cdots")))
;; (define-key abbrev-prefix-map "~" (lambda () (interactive) (insert "\\cong")))
;; (define-key abbrev-prefix-map "=" (lambda () (interactive) (insert "\\equiv")))
;; (define-key abbrev-prefix-map ">" (lambda () (interactive) (insert "\\geq")))
;; (define-key abbrev-prefix-map "<" (lambda () (interactive) (insert "\\leq")))
;; (define-key abbrev-prefix-map "v<" (lambda () (interactive) (insert "\\leftarrow")))
;; (define-key abbrev-prefix-map "v>" (lambda () (interactive) (insert "\\rightarrow")))
;; (define-key abbrev-prefix-map "vp" (lambda () (interactive) (insert "\\varpi")))
;; (define-key abbrev-prefix-map "vf" (lambda () (interactive) (insert "\\varphi")))
;; (define-key abbrev-prefix-map "ve" (lambda () (interactive) (insert "\\varepsilon")))
;; (define-key abbrev-prefix-map "W" (lambda () (interactive) (insert "\\Omega")))
;; (define-key abbrev-prefix-map "Y" (lambda () (interactive) (insert "\\Psi")))
;; (define-key abbrev-prefix-map "F" (lambda () (interactive) (insert "\\Phi")))
;; (define-key abbrev-prefix-map "U" (lambda () (interactive) (insert "\\Upsilon")))
;; (define-key abbrev-prefix-map "S" (lambda () (interactive) (insert "\\Sigma")))
;; (define-key abbrev-prefix-map "P" (lambda () (interactive) (insert "\\Pi")))
;; (define-key abbrev-prefix-map "X" (lambda () (interactive) (insert "\\Xi")))
;; (define-key abbrev-prefix-map "L" (lambda () (interactive) (insert "\\Lambda")))
;; (define-key abbrev-prefix-map "J" (lambda () (interactive) (insert "\\Theta")))
;; (define-key abbrev-prefix-map "G" (lambda () (interactive) (insert "\\Gamma")))
;; (define-key abbrev-prefix-map "D" (lambda () (interactive) (insert "\\Delta")))
;; (define-key abbrev-prefix-map "w" (lambda () (interactive) (insert "\\omega")))
;; (define-key abbrev-prefix-map "y" (lambda () (interactive) (insert "\\psi")))
;; (define-key abbrev-prefix-map "q" (lambda () (interactive) (insert "\\chi")))
;; (define-key abbrev-prefix-map "f" (lambda () (interactive) (insert "\\phi")))
;; (define-key abbrev-prefix-map "u" (lambda () (interactive) (insert "\\upsilon")))
;; (define-key abbrev-prefix-map "t" (lambda () (interactive) (insert "\\tau")))
;; (define-key abbrev-prefix-map "s" (lambda () (interactive) (insert "\\sigma")))
;; (define-key abbrev-prefix-map "r" (lambda () (interactive) (insert "\\rho")))
;; (define-key abbrev-prefix-map "p" (lambda () (interactive) (insert "\\pi")))
;; (define-key abbrev-prefix-map "x" (lambda () (interactive) (insert "\\xi")))
;; (define-key abbrev-prefix-map "n" (lambda () (interactive) (insert "\\nu")))
;; (define-key abbrev-prefix-map "m" (lambda () (interactive) (insert "\\mu")))
;; (define-key abbrev-prefix-map "l" (lambda () (interactive) (insert "\\lambda")))
;; (define-key abbrev-prefix-map "k" (lambda () (interactive) (insert "\\kappa")))
;; (define-key abbrev-prefix-map "j" (lambda () (interactive) (insert "\\theta")))
;; (define-key abbrev-prefix-map "h" (lambda () (interactive) (insert "\\eta")))
;; (define-key abbrev-prefix-map "z" (lambda () (interactive) (insert "\\zeta")))
;; (define-key abbrev-prefix-map "e" (lambda () (interactive) (insert "\\epsilon")))
;; (define-key abbrev-prefix-map "E" (lambda () (interactive) (insert "\\exists")))
;; (define-key abbrev-prefix-map "D" (lambda () (interactive) (insert "\\Delta")))
;; (define-key abbrev-prefix-map "d" (lambda () (interactive) (insert "\\delta")))
;; (define-key abbrev-prefix-map "g" (lambda () (interactive) (insert "\\gamma")))
;; (define-key abbrev-prefix-map "b" (lambda () (interactive) (insert "\\beta")))
;; (define-key abbrev-prefix-map "+" (lambda () (interactive) (insert "\\sum")))
;; (define-key abbrev-prefix-map "0" (lambda () (interactive) (insert "\\circ")))
;; (define-key abbrev-prefix-map "c" 'read-tex-complete)
;; (define-key abbrev-prefix-map (kbd "<return>") (lambda () (interactive) (insert "\n\\item{}\n")))

(setq tex-def-map (make-sparse-keymap))
(define-key tex-def-map [?\C-c ?f] #'tex-toggle-follow)

;;;###autoload
(defvar tex-follow-up-or-not nil
  "Variable to determine the tex follow mode")

;;;###autoload
(defvar tex-def-alist nil
  "An associative list to store the defs found in a tex file.")

;;;###autoload
(defvar tex-old-pos 0
  "The old position to go back to.")

;; use pdf-tools to open document
(after! (latex tex)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t))

;; my custom view function
(map! :map TeX-mode-map
      "C-c C-v" 'TeX-view)

;; hack the original function
(after! pdf-sync
  ;; (fset 'pdf-sync-forward-search 'durand-pdf-sync-forward-search)
  (set-popup-rule! "^\\*Outline*"
    :side 'bottom
    :size '+popup-shrink-to-fit))
(setq pdf-sync-forward-display-action '((display-buffer-same-window))
      pdf-sync-backward-display-action '((display-buffer-same-window)))

(defadvice! use-same-window-a (orig-fn)
  :around #'TeX-pdf-tools-sync-view
  (cl-letf (((symbol-function #'pop-to-buffer)
             (symbol-function #'switch-to-buffer)))
    (funcall orig-fn)))

;; clear `org-capture-plist' after each capture
(add-hook! 'org-capture-after-finalize-hook (setq org-capture-plist nil))

;;* electric math. NOTE: This is redundant, as doom uses smartparens to manage
;;* pairs.
;;
;; (add-hook! 'plain-TeX-mode-hook
;;   (set (make-local-variable 'TeX-electric-math)
;;        (cons "$" "$")))
;; (add-hook! 'LaTeX-mode-hook
;;   (set (make-local-variable 'TeX-electric-math)
;;        (cons "\\(" "\\)")))

(map! :map LaTeX-mode-map :i [?$] (lambda! (sp-insert-pair "\\(")))

;; (add-hook! (TeX-mode LaTeX-mode) 'turn-off-smartparens-mode)

;; insert display equation symbols

;;;###autoload
(defun durand-insert-display-equation ()
  "Insert display equation symbols."
  (interactive)
  (insert "\\[\\]")
  (goto-char (- (point) 2)))

(add-hook! 'LaTeX-mode-hook
           'LaTeX-math-mode)

(setq! LaTeX-math-list '((?$ durand-insert-display-equation)))

;; solve yasnippet and company conflicts
;;;###autoload
(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common-or-cycle nil)))
    (yas-expand)))

(map! :map LaTeX-mode-map
      :i
      [remap company-complete-common-or-cycle]
      'company-yasnippet-or-completion)

;; LaTeX section height
(setq! font-latex-fontify-sectioning 1.3)



;; ARCHIVE

;; (add-hook 'company-mode-hook
;;           (lambda ()
;;             (define-key TeX-mode-map [remap 'company-complete-common]
;;               'company-yasnippet-or-completion)
;;             ;; (substitute-key-definition
;;             ;;  'company-complete-common
;;             ;;  'company-yasnippet-or-completion
;;             ;;  TeX-mode-map)
;;             ))
