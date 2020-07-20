;;; ui/durand-modeline/autoload/light.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui modeline +light)

;;;###autoload
(defsubst +modeline-evil-state ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (+modeline-modal-icon
     (let ((tag (evil-state-property evil-state :tag t)))
       (if (stringp tag) tag (funcall tag)))
     (cond
      ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
      ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
      ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
      ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
      ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
      ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
      ((evil-replace-state-p) 'doom-modeline-evil-replace-state)
      (t 'doom-modeline-evil-normal-state))
     (evil-state-property evil-state :name t))))

;;;###autoload
(defun durand-modeline-position ()
  "Almost the same as `doom-modeline-segment--buffer-position',
except when in `org-agenda-mode' it uses `org-agenda-show-blocks-number' instead."
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (let* ((active (+modeline-active))
           (po (org-agenda-show-blocks-number))
           (face (if active 'mode-line 'mode-line-inactive))
           (mouse-face 'mode-line-highlight))
      (concat
       "  "
       (propertize po
                   'face face
                   'help-echo "org agenda block position"
                   'mouse-face mouse-face))))
   (t
    "  %l:%C %p  ")))

;;;###autoload
(defun +modeline-icon (icon-set icon-name unicode text &rest args)
  "Display icon of ICON-NAME with ARGS in mode-line.

ICON-SET includes `octicon', `faicon', `material', `alltheicons' and `fileicon'.
UNICODE is the unicode char fallback. TEXT is the ASCII char fallback.
ARGS is same as `all-the-icons-octicon' and others."
  (let ((face (or (plist-get args :face) 'mode-line)))
    (or
     ;; Icons
     (when (and icon-name
                (not (string-empty-p icon-name)))
       (let ((icon (pcase icon-set
                     ('octicon
                      (apply #'all-the-icons-octicon icon-name args))
                     ('faicon
                      (apply #'all-the-icons-faicon icon-name args))
                     ('material
                      (apply #'all-the-icons-material icon-name args))
                     ('alltheicon
                      (apply #'all-the-icons-alltheicon icon-name args))
                     ('fileicon
                      (apply #'all-the-icons-fileicon icon-name args)))))
         (+modeline-propertize-icon icon face)))
     ;; Unicode fallback
     (and unicode
          (not (string-empty-p unicode))
          (char-displayable-p (string-to-char unicode))
          (propertize unicode 'face face))
     ;; ASCII text
     (and text (propertize text 'face face)))))

;;;###autoload
(defun +modeline-propertize-icon (icon &optional face)
  "Propertize the ICON with the specified FACE.

The face should be the first attribute, or the font family may be overridden.
So convert the face \":family XXX :height XXX :inherit XXX\" to
\":inherit XXX :family XXX :height XXX\".
See https://github.com/seagle0128/doom-modeline/issues/301."
  (when-let* ((props (and icon (get-text-property 0 'face icon)))
              (family (plist-get props :family))
              (height (plist-get props :height))
              (face (or face (plist-get props :inherit)))
              (new-face (append `(:inherit ,face)
                                `(:family ,family)
                                `(:height ,height))))
    (propertize icon 'face new-face)))

;;;###autoload
(defun +modeline-modal-icon (text face help-echo)
  "Adapted from `doom-modeline-modal-icon'.
For TEXT, FACE, and HELP-ECHO see the documentation
of the function cited."
  (propertize (+modeline-icon
               'material
               "fiber_manual_record"
               "●"
               text
               :face (if (+modeline-active) face 'mode-line-inactive)
               :v-adjust -0.225)
              'help-echo help-echo))

;;;###autoload
(defface doom-modeline-evil-emacs-state
  '((t (:inherit (font-lock-builtin-face bold))))
  "Face for the Emacs state tag in evil state indicator."
  :group 'doom-modeline-faces)

;;;###autoload
(defface doom-modeline-evil-insert-state
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face for the insert state tag in evil state indicator."
  :group 'doom-modeline-faces)

;;;###autoload
(defface doom-modeline-evil-motion-state
  '((t :inherit (font-lock-doc-face bold) :slant normal))
  "Face for the motion state tag in evil state indicator."
  :group 'doom-modeline-faces)

;;;###autoload
(defface doom-modeline-evil-normal-state
  '((t (:inherit doom-modeline-info)))
  "Face for the normal state tag in evil state indicator."
  :group 'doom-modeline-faces)

;;;###autoload
(defface doom-modeline-evil-operator-state
  '((t (:inherit doom-modeline-buffer-file)))
  "Face for the operator state tag in evil state indicator."
  :group 'doom-modeline-faces)

;;;###autoload
(defface doom-modeline-evil-visual-state
  '((t (:inherit doom-modeline-warning)))
  "Face for the visual state tag in evil state indicator."
  :group 'doom-modeline-faces)

;;;###autoload
(defface doom-modeline-evil-replace-state
  '((t (:inherit doom-modeline-urgent)))
  "Face for the replace state tag in evil state indicator."
  :group 'doom-modeline-faces)
