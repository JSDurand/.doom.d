;;; tools/durand-pdf/autoload.el -*- lexical-binding: t; -*-

;;* pdf view scrolling

;;;###autoload
(defun durand-pdf-scroll-up-or-next-page ()
  "Scroll half a page instead of nearly a page."
  (interactive)
  (durand-buffer-scroll 'up nil nil))

;;;###autoload
(defun durand-pdf-scroll-down-or-previous-page ()
  "Scroll half a page instead of nearly a page."
  (interactive)
  (durand-buffer-scroll 'down nil nil))

