;;; term/durand-eshell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun durand/eshell-complete-recent-dir ()
  "Choose a recent directory to go to."
  (interactive)
  (let* ((recent-dirs (ring-elements eshell-last-dir-ring))
         (chosen-dir (completing-read
                      "Chois un directoire:"
                      recent-dirs nil t)))
    (goto-char (point-max))
    (insert chosen-dir)
    (eshell-send-input)))
