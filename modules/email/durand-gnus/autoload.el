;;; email/durand-gnus/autoload.el -*- lexical-binding: t; -*-

;; Some helper functions to facilitate my navigations in gnus.

;;;###autoload
(defun recenter-to-top ()
  "Recenter to the top."
  (interactive)
  (recenter 0))

;;;###autoload
(defun recenter-to-middle ()
  "Recenter to the middle."
  (interactive)
  (recenter (/ (window-body-width) 2)))

;;;###autoload
(defun recenter-to-bottom ()
  "Recenter to the bottom."
  (interactive)
  (recenter -1))
