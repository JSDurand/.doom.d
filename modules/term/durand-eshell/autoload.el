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

;;;###autoload
(defun durand-eshell-goto-last-output-end ()
  "Go to the end of the last output.
Since the prompt is an output, this goes to the prompt."
  (interactive)
  (when (and eshell-last-output-end
             (number-or-marker-p eshell-last-output-end))
    (goto-char eshell-last-output-end)))
