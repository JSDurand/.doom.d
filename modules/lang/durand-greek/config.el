;;; lang/durand-greek/config.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar durand-greek-replace-list nil
  "A list of replacement specifications.
See `durand-greek-search-replace-generic' for more details.")

(setq durand-greek-replace-list
      '(("\\([ahw]\\))/|"
         (("a" "ᾄ")
          ("h" "ᾔ")
          ("w" "ᾤ"))
         :sub-exp 1)
        ("\\([ahw]\\)(/|"
         (("a" "ᾅ")
          ("h" "ᾕ")
          ("w" "ᾥ"))
         :sub-exp 1)
        ("\\([ahw]\\))\\\\|"
         (("a" "ᾂ")
          ("h" "ᾒ")
          ("w" "ᾢ"))
         :sub-exp 1)
        ("\\([ahw]\\)(\\\\|"
         (("a" "ᾃ")
          ("h" "ᾓ")
          ("w" "ᾣ"))
         :sub-exp 1)
        ("\\([ahw]\\))=|"
         (("a" "ᾆ")
          ("h" "ᾖ")
          ("w" "ᾦ"))
         :sub-exp 1)
        ("\\([ahw]\\)(=|"
         (("a" "ᾇ")
          ("h" "ᾗ")
          ("w" "ᾧ"))
         :sub-exp 1)
        ("\\([aeiouhw]\\))/"
         (("a" "ἄ")
          ("e" "ἔ")
          ("i" "ἴ")
          ("o" "ὄ")
          ("u" "ὔ")
          ("h" "ἤ")
          ("w" "ὤ"))
         :sub-exp 1)
        ("\\([aeiouhw]\\)(/"
         (("a" "ἅ")
          ("e" "ἕ")
          ("i" "ἵ")
          ("o" "ὅ")
          ("u" "ὕ")
          ("h" "ἥ")
          ("w" "ὥ"))
         :sub-exp 1)
        ("\\([aeiouhw]\\))\\\\"
         (("a" "ἂ")
          ("e" "ἒ")
          ("i" "ἲ")
          ("o" "ὂ")
          ("u" "ὒ")
          ("h" "ἢ")
          ("w" "ὢ"))
         :sub-exp 1)
        ("\\([aeiouhw]\\)(\\\\"
         (("a" "ἃ")
          ("e" "ἓ")
          ("i" "ἳ")
          ("o" "ὃ")
          ("u" "ὓ")
          ("h" "ἣ")
          ("w" "ὣ"))
         :sub-exp 1)
        ("\\([aiuhw]\\))="
         (("a" "ἆ")
          ("i" "ἶ")
          ("u" "ὖ")
          ("h" "ἦ")
          ("w" "ὦ"))
         :sub-exp 1)
        ("\\([aiuhw]\\)(="
         (("a" "ἇ")
          ("i" "ἷ")
          ("u" "ὗ")
          ("h" "ἧ")
          ("w" "ὧ"))
         :sub-exp 1)
        ("\\([ahw]\\))|"
         (("a" "ᾀ")
          ("h" "ᾐ")
          ("w" "ᾠ"))
         :sub-exp 1)
        ("\\([ahw]\\)(|"
         (("a" "ᾁ")
          ("h" "ᾑ")
          ("w" "ᾡ"))
         :sub-exp 1)
        ("\\([ahw]\\)/|"
         (("a" "ᾴ")
          ("h" "ῄ")
          ("w" "ῴ"))
         :sub-exp 1)
        ("\\([ahw]\\)=|"
         (("a" "ᾷ")
          ("h" "ῇ")
          ("w" "ῷ"))
         :sub-exp 1)
        ("\\([ahw]\\)\\\\|"
         (("a" "ᾲ")
          ("h" "ῂ")
          ("w" "ῲ"))
         :sub-exp 1)
        ("\\([aiueohw]\\))"
         (("a" "ἀ")
          ("e" "ἐ")
          ("o" "ὀ")
          ("i" "ἰ")
          ("u" "ὐ")
          ("h" "ἠ")
          ("w" "ὠ"))
         :sub-exp 1)
        ("\\([aiueohwr]\\)("
         (("a" "ἁ")
          ("e" "ἑ")
          ("o" "ὁ")
          ("i" "ἱ")
          ("u" "ὑ")
          ("h" "ἡ")
          ("r" "ῥ")
          ("w" "ὡ"))
         :sub-exp 1)
        ("\\([aiuhw]\\)="
         (("a" "ᾶ")
          ("i" "ῖ")
          ("u" "ῦ")
          ("h" "ῆ")
          ("w" "ῶ"))
         :sub-exp 1)
        ("\\([aiueohw]\\)/"
         (("a" "ά")
          ("e" "έ")
          ("o" "ό")
          ("i" "ί")
          ("u" "ύ")
          ("h" "ή")
          ("w" "ώ"))
         :sub-exp 1)
        ("\\([aiueohw]\\)\\\\"
         (("a" "ὰ")
          ("e" "ὲ")
          ("o" "ὸ")
          ("i" "ὶ")
          ("u" "ὺ")
          ("h" "ὴ")
          ("w" "ὼ"))
         :sub-exp 1)
        ("\\([iu]\\)\\+/"
         (("i" "ΐ")
          ("u" "ΰ"))
         :sub-exp 1)
        ("\\([iu]\\)\\+\\\\"
         (("i" "ῒ")
          ("u" "ῢ"))
         :sub-exp 1)
        ("\\([iu]\\)\\+="
         (("i" "ῗ")
          ("u" "ῧ"))
         :sub-exp 1)
        ("\\([iu]\\)\\+"
         (("i" "ϊ")
          ("u" "ϋ"))
         :sub-exp 1)
        ("\\([ahw]\\)|"
         (("a" "ᾳ")
          ("h" "ῃ")
          ("w" "ῳ"))
         :sub-exp 1)
        ("a" "α")
        ("b" "β")
        ("c" "ξ")
        ("d" "δ")
        ("e" "ε")
        ("f" "φ")
        ("g" "γ")
        ("h" "η")
        ("i" "ι")
        ("k" "κ")
        ("l" "λ")
        ("m" "μ")
        ("n" "ν")
        ("o" "ο")
        ("p" "π")
        ("q" "θ")
        ("r" "ρ")
        ("t" "τ")
        ("u" "υ")
        ("w" "ω")
        ("x" "χ")
        ("y" "ψ")
        ("z" "ζ")
        ("\\(s\\)[[:alpha:]]"
         "σ"
         :rep-exp 1)
        ("s" "ς")
        ("\\*\\(.\\)"
         upcase
         :sub-exp 1)))

(define-derived-mode durand-greek-search-mode special-mode "Greek Search"
  "To show search results")

(map! :map durand-greek-search-mode-map
      :n [?j] 'durand-goto-greek-search-result
      :n [?n] 'forward-line
      :n [?p] (lambda! (forward-line -1)))
;; (define-key durand-greek-search-mode-map [?j] #'durand-goto-greek-search-result)
;; (define-key durand-greek-search-mode-map [?n] #'forward-line)
;; (define-key durand-greek-search-mode-map [?p] (lambda! (forward-line -1)))

;;;###autoload
(defvar durand-search-greek-last-search nil
  "This variable holds the last searched terms to search in greek.")

(define-derived-mode durand-greek-mode text-mode "Durand Greek"
  "Provides a transformation function to transform English code letters to ancient greek.")

;; keys
(map! :map durand-greek-mode-map
      :niv [?ù] 'durand-transform-code-to-greek
      :i [?§] (lambda! (insert "\\"))
      :i [?:] (lambda! (insert "/"))
      :i [?/] (lambda! (insert ":"))
      :i [?ç] (lambda! (insert "."))
      :i [?\M-\r] 'org-insert-item)
;; (define-key durand-greek-mode-map [?ù] #'durand-transform-code-to-greek)
;; (define-key durand-greek-mode-map [?§] (lambda () (interactive) (insert "\\")))
;; (define-key durand-greek-mode-map [?:] (lambda () (interactive) (insert "/")))
;; (define-key durand-greek-mode-map [?/] (lambda () (interactive) (insert ":")))
;; (define-key durand-greek-mode-map [?\ç] (lambda () (interactive) (insert "·")))
;; (define-key durand-greek-mode-map [?ç] #'durand-search-greek)
;; (define-key durand-greek-mode-map [?%] #'durand-search-last-greek)
;; (define-key durand-greek-mode-map [?\M-\r] #'org-insert-item) ; M-RET

(add-hook 'durand-greek-mode-hook 'turn-on-orgtbl)
