;;; tools/sese/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun sese-reset-start ()
  "Reset the timestamp of the start of the subtitle."
  (interactive)
  (sese-set-marker-pos 1))

;;;###autoload
(defun sese-reset-end ()
  "Reset the timestamp of the end of the subtitle."
  (interactive)
  (sese-set-marker-pos 4))
