;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! evil-snipe :disable t)
(disable-packages! evil-snipe which-key org-bookmark-heading durand-cat undo-tree)
;; (package! org-pdfview)

;; (package! durand-cat :recipe (:host github :repo "JSDurand/durand-cat"))

(package! wrap-region)

(package! emacs-opencc :recipe (:host github :repo "xuchunyang/emacs-opencc"))

(package! outline-magic :recipe (:host github :repo "tj64/outline-magic"))

(package! mu4e-alert)
