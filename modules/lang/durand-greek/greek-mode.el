;;; -*- lexical-binding: t; -*-

;; A major-mode for entering ancient greek letters. It does not provide shortcuts for
;; entering greek letters, though; instead, it provides a function for transforming the
;; corresponding words in english letters to the ancient greek words.

;; For example, it will transform this e)pidei=cai to this ἐπιδεῖξαι.

;; I think this is more convenient than using short-cuts to enter, which is a little
;; cumbersome in my opinion.

(require 'evil)

;;;###autoload
(evil-define-operator durand-transform-code-to-greek (beg end type)
  "Transform code letters into ancient greek."
  :move-point nil
  :repeat t
  (interactive "<R>")
  (pcase type
    ('block
        (apply-on-rectangle 'durand-transform-each-in-rectangle beg end))
    (_
     (durand-transform-from-beg-to-end beg end))))

;;;###autoload
(defun durand-transform-each-in-rectangle (startcol endcol)
  "Starting from startcol, transform to endcol."
  (let (start end)
    (set-mark-command nil)
    (move-to-column startcol)
    (setq start (point))
    (move-to-column endcol)
    (setq end (point))
    (goto-char start)
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))/|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾄ")
                         ("h" "ᾔ")
                         ("w" "ᾤ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(/|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾅ")
                         ("h" "ᾕ")
                         ("w" "ᾥ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))\\\\|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾂ")
                         ("h" "ᾒ")
                         ("w" "ᾢ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(\\\\|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾃ")
                         ("h" "ᾓ")
                         ("w" "ᾣ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))=|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾆ")
                         ("h" "ᾖ")
                         ("w" "ᾦ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(=|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾇ")
                         ("h" "ᾗ")
                         ("w" "ᾧ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\))/" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἄ")
                         ("e" "ἔ")
                         ("i" "ἴ")
                         ("o" "ὄ")
                         ("u" "ὔ")
                         ("h" "ἤ")
                         ("w" "ὤ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\)(/" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἅ")
                         ("e" "ἕ")
                         ("i" "ἵ")
                         ("o" "ὅ")
                         ("u" "ὕ")
                         ("h" "ἥ")
                         ("w" "ὥ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\))\\\\" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἂ")
                         ("e" "ἒ")
                         ("i" "ἲ")
                         ("o" "ὂ")
                         ("u" "ὒ")
                         ("h" "ἢ")
                         ("w" "ὢ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\)(\\\\" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἃ")
                         ("e" "ἓ")
                         ("i" "ἳ")
                         ("o" "ὃ")
                         ("u" "ὓ")
                         ("h" "ἣ")
                         ("w" "ὣ")))))
    (save-excursion
      (while (re-search-forward "\\([aiuhw]\\))=" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἆ")
                         ("i" "ἶ")
                         ("u" "ὖ")
                         ("h" "ἦ")
                         ("w" "ὦ")))))
    (save-excursion
      (while (re-search-forward "\\([aiuhw]\\)(=" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἇ")
                         ("i" "ἷ")
                         ("u" "ὗ")
                         ("h" "ἧ")
                         ("w" "ὧ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾀ")
                         ("h" "ᾐ")
                         ("w" "ᾠ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾁ")
                         ("h" "ᾑ")
                         ("w" "ᾡ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)/|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾴ")
                         ("h" "ῄ")
                         ("w" "ῴ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)\\\\|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾲ")
                         ("h" "ῂ")
                         ("w" "ῲ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)=|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾷ")
                         ("h" "ῇ")
                         ("w" "ῷ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohw]\\))" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἀ")
                         ("e" "ἐ")
                         ("o" "ὀ")
                         ("i" "ἰ")
                         ("u" "ὐ")
                         ("h" "ἠ")
                         ("w" "ὠ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohwr]\\)(" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἁ")
                         ("e" "ἑ")
                         ("o" "ὁ")
                         ("i" "ἱ")
                         ("u" "ὑ")
                         ("h" "ἡ")
                         ("w" "ὡ")
                         ("r" "ῥ")))))
    (save-excursion
      (while (re-search-forward "\\([aiuhw]\\)=" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾶ")
                         ("i" "ῖ")
                         ("u" "ῦ")
                         ("h" "ῆ")
                         ("w" "ῶ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohw]\\)/" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ά")
                         ("e" "έ")
                         ("o" "ό")
                         ("i" "ί")
                         ("u" "ύ")
                         ("h" "ή")
                         ("w" "ώ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohw]\\)\\\\" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ὰ")
                         ("e" "ὲ")
                         ("o" "ὸ")
                         ("i" "ὶ")
                         ("u" "ὺ")
                         ("h" "ὴ")
                         ("w" "ὼ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+/" end t)
        (replace-match (pcase (match-string 1)
                         ("i" "ΐ")
                         ("u" "ΰ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+\\\\" end t)
        (replace-match (pcase (match-string 1)
                         ("i" "ῒ")
                         ("u" "ῢ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+=" end t)
        (replace-match (pcase (match-string 1)
                         ("i" "ῗ")
                         ("u" "ῧ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+" end t)
        (replace-match (pcase (match-string 1)
                         ("i" "ϊ")
                         ("u" "ϋ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)|" end t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾳ")
                         ("h" "ῃ")
                         ("w" "ῳ")))))
    (cl-mapc
     (lambda (x y)
       (save-excursion
         (while (search-forward x end t)
           (replace-match y))))
     '("a" "b" "c" "d" "e" "f" "g" "h" "i" "k" "l" "m" "n" "o" "p" "q" "r" "t" "u" "w" "x" "y" "z")
     '("α" "β" "ξ" "δ" "ε" "φ" "γ" "η" "ι" "κ" "λ" "μ" "ν" "ο" "π" "θ" "ρ" "τ" "υ" "ω" "χ" "ψ" "ζ"))
    (save-excursion
      (while (re-search-forward "\\(s\\)[[:alpha:]]" end t)
        (replace-match "σ" nil nil nil 1)))
    (save-excursion
      (while (re-search-forward "s" end t)
        (replace-match "ς" nil nil nil 0)))
    (save-excursion
      (while (search-forward "*" end t)
        (delete-region (1- (point)) (point))
        (upcase-region (point) (1+ (point)))))
    (set-mark-command '(4))))

;;;###autoload
(defun durand-transform-from-beg-to-end (beg end)
  "Do what the name says."
  ;; after replacing characters, the original ending position is wrong, so I should use markers
  (let ((orig-marker (make-marker))
        (end-marker (make-marker)))
    (set-marker orig-marker (point))
    (set-marker end-marker end)
    (goto-char beg)
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))/|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾄ")
                         ("h" "ᾔ")
                         ("w" "ᾤ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(/|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾅ")
                         ("h" "ᾕ")
                         ("w" "ᾥ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))\\\\|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾂ")
                         ("h" "ᾒ")
                         ("w" "ᾢ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(\\\\|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾃ")
                         ("h" "ᾓ")
                         ("w" "ᾣ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))=|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾆ")
                         ("h" "ᾖ")
                         ("w" "ᾦ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(=|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾇ")
                         ("h" "ᾗ")
                         ("w" "ᾧ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\))/" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἄ")
                         ("e" "ἔ")
                         ("i" "ἴ")
                         ("o" "ὄ")
                         ("u" "ὔ")
                         ("h" "ἤ")
                         ("w" "ὤ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\)(/" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἅ")
                         ("e" "ἕ")
                         ("i" "ἵ")
                         ("o" "ὅ")
                         ("u" "ὕ")
                         ("h" "ἥ")
                         ("w" "ὥ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\))\\\\" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἂ")
                         ("e" "ἒ")
                         ("i" "ἲ")
                         ("o" "ὂ")
                         ("u" "ὒ")
                         ("h" "ἢ")
                         ("w" "ὢ")))))
    (save-excursion
      (while (re-search-forward "\\([aeiouhw]\\)(\\\\" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἃ")
                         ("e" "ἓ")
                         ("i" "ἳ")
                         ("o" "ὃ")
                         ("u" "ὓ")
                         ("h" "ἣ")
                         ("w" "ὣ")))))
    (save-excursion
      (while (re-search-forward "\\([aiuhw]\\))=" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἆ")
                         ("i" "ἶ")
                         ("u" "ὖ")
                         ("h" "ἦ")
                         ("w" "ὦ")))))
    (save-excursion
      (while (re-search-forward "\\([aiuhw]\\)(=" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἇ")
                         ("i" "ἷ")
                         ("u" "ὗ")
                         ("h" "ἧ")
                         ("w" "ὧ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\))|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾀ")
                         ("h" "ᾐ")
                         ("w" "ᾠ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)(|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾁ")
                         ("h" "ᾑ")
                         ("w" "ᾡ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)/|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾴ")
                         ("h" "ῄ")
                         ("w" "ῴ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)\\\\|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾲ")
                         ("h" "ῂ")
                         ("w" "ῲ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)=|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾷ")
                         ("h" "ῇ")
                         ("w" "ῷ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohw]\\))" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἀ")
                         ("e" "ἐ")
                         ("o" "ὀ")
                         ("i" "ἰ")
                         ("u" "ὐ")
                         ("h" "ἠ")
                         ("w" "ὠ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohwr]\\)(" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ἁ")
                         ("e" "ἑ")
                         ("o" "ὁ")
                         ("i" "ἱ")
                         ("u" "ὑ")
                         ("h" "ἡ")
                         ("r" "ῥ")
                         ("w" "ὡ")))))
    (save-excursion
      (while (re-search-forward "\\([aiuhw]\\)=" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾶ")
                         ("i" "ῖ")
                         ("u" "ῦ")
                         ("h" "ῆ")
                         ("w" "ῶ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohw]\\)/" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ά")
                         ("e" "έ")
                         ("o" "ό")
                         ("i" "ί")
                         ("u" "ύ")
                         ("h" "ή")
                         ("w" "ώ")))))
    (save-excursion
      (while (re-search-forward "\\([aiueohw]\\)\\\\" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ὰ")
                         ("e" "ὲ")
                         ("o" "ὸ")
                         ("i" "ὶ")
                         ("u" "ὺ")
                         ("h" "ὴ")
                         ("w" "ὼ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+/" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("i" "ΐ")
                         ("u" "ΰ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+\\\\" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("i" "ῒ")
                         ("u" "ῢ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+=" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("i" "ῗ")
                         ("u" "ῧ")))))
    (save-excursion
      (while (re-search-forward "\\([iu]\\)\\+" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("i" "ϊ")
                         ("u" "ϋ")))))
    (save-excursion
      (while (re-search-forward "\\([ahw]\\)|" (marker-position end-marker) t)
        (replace-match (pcase (match-string 1)
                         ("a" "ᾳ")
                         ("h" "ῃ")
                         ("w" "ῳ")))))
    (cl-mapc
     (lambda (x y)
       (save-excursion
         (while (search-forward x (marker-position end-marker) t)
           (replace-match y))))
     '("a" "b" "c" "d" "e" "f" "g" "h" "i" "k" "l" "m" "n" "o" "p" "q" "r" "t" "u" "w" "x" "y" "z")
     '("α" "β" "ξ" "δ" "ε" "φ" "γ" "η" "ι" "κ" "λ" "μ" "ν" "ο" "π" "θ" "ρ" "τ" "υ" "ω" "χ" "ψ" "ζ"))
    (save-excursion
      (while (re-search-forward "\\(s\\)[[:alpha:]]" (marker-position end-marker) t)
        (replace-match "σ" nil nil nil 1)))
    (save-excursion
      (while (re-search-forward "s" (marker-position end-marker) t)
        (replace-match "ς" nil nil nil 0)))
    (save-excursion
      (while (search-forward "*" (marker-position end-marker) t)
        (delete-region (1- (point)) (point))
        (upcase-region (point) (1+ (point)))))
    (goto-char (marker-position orig-marker))
    (set-marker orig-marker nil)
    (set-marker end-marker nil)))

;;;###autoload
(defun durand-transform-string (str)
  "Transform the string so that we can search the buffer with the code letters."
  (save-excursion
    (while (string-match "\\([ahw]\\))/|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾄ")
                                 ("h" "ᾔ")
                                 ("w" "ᾤ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)(/|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾅ")
                                 ("h" "ᾕ")
                                 ("w" "ᾥ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\))\\\\|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾂ")
                                 ("h" "ᾒ")
                                 ("w" "ᾢ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)(\\\\|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾃ")
                                 ("h" "ᾓ")
                                 ("w" "ᾣ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\))=|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾆ")
                                 ("h" "ᾖ")
                                 ("w" "ᾦ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)(=|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾇ")
                                 ("h" "ᾗ")
                                 ("w" "ᾧ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aeiouhw]\\))/" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἄ")
                                 ("e" "ἔ")
                                 ("i" "ἴ")
                                 ("o" "ὄ")
                                 ("u" "ὔ")
                                 ("h" "ἤ")
                                 ("w" "ὤ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aeiouhw]\\)(/" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἅ")
                                 ("e" "ἕ")
                                 ("i" "ἵ")
                                 ("o" "ὅ")
                                 ("u" "ὕ")
                                 ("h" "ἥ")
                                 ("w" "ὥ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aeiouhw]\\))\\\\" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἂ")
                                 ("e" "ἒ")
                                 ("i" "ἲ")
                                 ("o" "ὂ")
                                 ("u" "ὒ")
                                 ("h" "ἢ")
                                 ("w" "ὢ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aeiouhw]\\)(\\\\" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἃ")
                                 ("e" "ἓ")
                                 ("i" "ἳ")
                                 ("o" "ὃ")
                                 ("u" "ὓ")
                                 ("h" "ἣ")
                                 ("w" "ὣ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiuhw]\\))=" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἆ")
                                 ("i" "ἶ")
                                 ("u" "ὖ")
                                 ("h" "ἦ")
                                 ("w" "ὦ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiuhw]\\)(=" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἇ")
                                 ("i" "ἷ")
                                 ("u" "ὗ")
                                 ("h" "ἧ")
                                 ("w" "ὧ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\))|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾀ")
                                 ("h" "ᾐ")
                                 ("w" "ᾠ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)(|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾁ")
                                 ("h" "ᾑ")
                                 ("w" "ᾡ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)/|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾴ")
                                 ("h" "ῄ")
                                 ("w" "ῴ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)=|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾷ")
                                 ("h" "ῇ")
                                 ("w" "ῷ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)\\\\|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾲ")
                                 ("h" "ῂ")
                                 ("w" "ῲ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiueohw]\\))" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἀ")
                                 ("e" "ἐ")
                                 ("o" "ὀ")
                                 ("i" "ἰ")
                                 ("u" "ὐ")
                                 ("h" "ἠ")
                                 ("w" "ὠ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiueohwr]\\)(" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ἁ")
                                 ("e" "ἑ")
                                 ("o" "ὁ")
                                 ("i" "ἱ")
                                 ("u" "ὑ")
                                 ("h" "ἡ")
                                 ("r" "ῥ")
                                 ("w" "ὡ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiuhw]\\)=" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾶ")
                                 ("i" "ῖ")
                                 ("u" "ῦ")
                                 ("h" "ῆ")
                                 ("w" "ῶ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiueohw]\\)/" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ά")
                                 ("e" "έ")
                                 ("o" "ό")
                                 ("i" "ί")
                                 ("u" "ύ")
                                 ("h" "ή")
                                 ("w" "ώ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([aiueohw]\\)\\\\" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ὰ")
                                 ("e" "ὲ")
                                 ("o" "ὸ")
                                 ("i" "ὶ")
                                 ("u" "ὺ")
                                 ("h" "ὴ")
                                 ("w" "ὼ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([iu]\\)\\+/" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("i" "ΐ")
                                 ("u" "ΰ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([iu]\\)\\+\\\\" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("i" "ῒ")
                                 ("u" "ῢ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([iu]\\)\\+=" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("i" "ῗ")
                                 ("u" "ῧ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([iu]\\)\\+" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("i" "ϊ")
                                 ("u" "ϋ"))
                               nil nil str))))
  (save-excursion
    (while (string-match "\\([ahw]\\)|" str)
      (setf str (replace-match (pcase (match-string 1 str)
                                 ("a" "ᾳ")
                                 ("h" "ῃ")
                                 ("w" "ῳ"))
                               nil nil str))))
  (cl-mapc
   (lambda (x y)
     (save-excursion
       (while (string-match x str)
         (setf str (replace-match y nil nil str)))))
   '("a" "b" "c" "d" "e" "f" "g" "h" "i" "k" "l" "m" "n" "o" "p" "q" "r" "t" "u" "w" "x" "y" "z")
   '("α" "β" "ξ" "δ" "ε" "φ" "γ" "η" "ι" "κ" "λ" "μ" "ν" "ο" "π" "θ" "ρ" "τ" "υ" "ω" "χ" "ψ" "ζ"))
  (save-excursion
    (while (string-match "\\(s\\)[[:alpha:]]" str)
      (setf str (replace-match "σ" nil nil str 1))))
  (save-excursion
    (while (string-match "s" str)
      (setf str (replace-match "ς" nil nil str))))
  (save-excursion
    (while (string-match "\\*\\(.\\)" str)
      (setf str (replace-match (upcase (match-string 1 str)) nil nil str))))
  str)

;;;###autoload
(defvar durand-search-greek-overlays nil
  "A list of overlays to be cleared later")

;;;###autoload
(defun durand-search-greek (input &optional already-transformed)
  "Search the buffer with the input being interpreted as code letters for greek letters.
The search is insensitive to accent marks and capitalisation.
If ALREADY-TRANSFORMED is non-nil, then don't perform transformation."
  (interactive (list (read-from-minibuffer "Search for: ")))
  (let* ((decoded-input (durand-transform-string input))
         (insensitive-input
          (cond
           ((null already-transformed)
            (cl-mapc
             (lambda (x y)
               (let ((start 0))
                 (while (setf start (string-match x decoded-input start))
                   (setf decoded-input (replace-match y nil nil decoded-input)
                         start (+ start (length y))))))
             '("α" "ε" "ι" "ο" "υ" "η" "ω" "ρ")
             '("[αάὰἁἀἅἄἃἂᾅᾄᾃᾂᾶἇἆᾇᾆᾳ]"
               "[εέὲἑἐἕἔἓἒ]"
               "[ιίὶἱἰἵἴἳἲῖἷἶϊΐῒῗ]"
               "[οόὸὁὀὅὄὃὂ]"
               "[υύὺὑὐὕὔὓὒῦὗὖϋΰῢῧ]"
               "[ηἠἡήἤἥὴἢἣῆἦἧῃᾐᾑῄᾔᾕῂᾒᾓῇᾖᾗ]"
               "[ωὠὡώὤὥὼὢὣῶὦὧῳᾠᾡῴᾤᾥῲᾢᾣῷᾦᾧ]"
               "[ρῥ]"))
            decoded-input)
           (t input)))
         (orig (point)))
    (push insensitive-input durand-search-greek-last-search)
    (unless (re-search-forward insensitive-input nil t)
      (goto-char (point-min))
      (unless (re-search-forward insensitive-input nil t)
        (goto-char orig)))
    (dolist (ov durand-search-greek-overlays)
      (delete-overlay ov)
      (setf durand-search-greek-overlays nil))
    (when (/= orig (point))
      (let ((ov (make-overlay (match-beginning 0)
                              (match-end 0))))
        (overlay-put ov 'face isearch-face)
        (push ov durand-search-greek-overlays)))))

;;;###autoload
(defun durand-show-search-greek (input &optional already-transformed)
  "Search the buffer with the input being interpreted as code letters for greek letters.
The search is insensitive to accent marks and capitalisation.
If ALREADY-TRANSFORMED is non-nil, then don't perform transformation."
  (interactive (list (read-from-minibuffer "Search for: ")))
  (let* ((decoded-input (durand-transform-string input))
         (insensitive-input
          (cond
           ((null already-transformed)
            (cl-mapc
             (lambda (x y)
               (let ((start 0))
                 (while (setf start (string-match x decoded-input start))
                   (setf decoded-input (replace-match y nil nil decoded-input)
                         start (+ start (length y))))))
             '("α" "ε" "ι" "ο" "υ" "η" "ω" "ρ")
             '("[αάὰἁἀἅἄἃἂᾅᾄᾃᾂᾶἇἆᾇᾆᾷᾳ]"
               "[εέὲἑἐἕἔἓἒ]"
               "[ιίὶἱἰἵἴἳἲῖἷἶϊΐῒῗ]"
               "[οόὸὁὀὅὄὃὂ]"
               "[υύὺὑὐὕὔὓὒῦὗὖϋΰῢῧ]"
               "[ηἠἡήἤἥὴἢἣῆἦἧῃᾐᾑῄᾔᾕῂᾒᾓῇᾖᾗ]"
               "[ωὠὡώὤὥὼὢὣῶὦὧῳᾠᾡῴᾤᾥῲᾢᾣῷᾦᾧ]"
               "[ρῥ]"))
            decoded-input)
           (t input)))
         res)
    (push insensitive-input durand-search-greek-last-search)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward insensitive-input nil t)
        (push (list (buffer-substring
                     (line-beginning-position)
                     (line-end-position))
                    (match-beginning 0)
                    (match-end 0)
                    (line-beginning-position))
              res))
      (setf res (cl-remove-duplicates
                 (nreverse res)
                 :test (lambda (x y) (= (cadddr x) (cadddr y))))))
    (dolist (ov durand-search-greek-overlays)
      (delete-overlay ov)
      (setf durand-search-greek-overlays nil))
    (dolist (item res)
      (let ((ov (make-overlay (cadr item)
                              (caddr item))))
        (overlay-put ov 'face isearch-face)
        (push ov durand-search-greek-overlays)))
    (with-current-buffer-window
     "*Search-results*" nil nil
     (dolist (item res)
       (let ((text (propertize (car item) 'pos (caddr item))))
         (insert text)
         (insert "\n")))
     (durand-greek-search-mode))
    (other-window 1)
    (durand-goto-greek-search-result (caddar res))))

;;;###autoload
(define-derived-mode durand-greek-search-mode special-mode "Greek Search"
  "To show search results")

;;;###autoload
(defun durand-goto-greek-search-result (&optional pos)
  "Go to the position corresponding to the search result at point."
  (interactive)
  (let ((dest (or pos
                  (get-text-property (point) 'pos)
                  (user-error "No \"pos\" information at point"))))
    (other-window 1)
    (goto-char dest)
    (org-show-context)
    (other-window 1)))

;;;###autoload
(define-key durand-greek-search-mode-map [?j] #'durand-goto-greek-search-result)
(define-key durand-greek-search-mode-map [?n] #'forward-line)
(define-key durand-greek-search-mode-map [?p] (lambda () (interactive) (forward-line -1)))

;;;###autoload
(defun durand-clear-overlays ()
  "Clear overlays"
  (interactive)
  (dolist (ov durand-search-greek-overlays)
    (delete-overlay ov)
    (setf durand-search-greek-overlays nil)))

;;;###autoload
(defvar durand-search-greek-last-search nil
  "This variable holds the last searched terms to search in greek.")

;;;###autoload
(defun durand-search-last-greek ()
  "Search the last searched term."
  (interactive)
  (cond
   (durand-search-greek-last-search
    (durand-search-greek
     (pop durand-search-greek-last-search)
     t))
   (t
    (message "No last searched term."))))

;; now the major mode
(define-derived-mode durand-greek-mode text-mode "Durand Greek"
  "Provides a transformation function to transform English code letters to ancient greek.")

;; keys
(define-key durand-greek-mode-map [?ù] #'durand-transform-code-to-greek)
(define-key durand-greek-mode-map [?§] (lambda () (interactive) (insert "\\")))
(define-key durand-greek-mode-map [?:] (lambda () (interactive) (insert "/")))
(define-key durand-greek-mode-map [?/] (lambda () (interactive) (insert ":")))
(define-key durand-greek-mode-map [?\ç] (lambda () (interactive) (insert "·")))
;; (define-key durand-greek-mode-map [?ç] #'durand-search-greek)
;; (define-key durand-greek-mode-map [?%] #'durand-search-last-greek)
(define-key durand-greek-mode-map [?\M-\r] #'org-insert-item) ; M-RET

;; hooks
(add-hook 'durand-greek-mode-hook 'turn-on-orgtbl)

;; greek-mode.el ends here
