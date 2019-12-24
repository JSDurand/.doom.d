;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! evil-snipe :disable t)
(disable-packages! evil-snipe)
;; (package! org-pdfview)

(package! wrap-region)

(package! outline-magic :recipe (:host github :repo "tj64/outline-magic"))

(package! mu4e-alert)
