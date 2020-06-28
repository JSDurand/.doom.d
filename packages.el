;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(disable-packages! evil-snipe org-bookmark-heading durand-cat undo-tree
                   flyspell-correct-popup)
;; (package! org-pdfview)

;; (package! durand-cat :recipe (:host github :repo "JSDurand/durand-cat"))

(package! wrap-region)

;; (package! emacs-opencc :recipe (:host github :repo "xuchunyang/emacs-opencc"))

(package! outline-magic :recipe (:host github :repo "tj64/outline-magic"))

(package! rg :recipe (:host github :repo "dajva/rg.el")
  :pin "87fed01062e9d9b1ecf948cc1997111ee75465fb")

(package! mu4e-alert)

;; try out vivendi
(package! modus-vivendi-theme)
(package! modus-operandi-theme)

;; why is rainbow-mode missing?
(package! rainbow-mode)
