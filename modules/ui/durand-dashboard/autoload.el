;;; ui/durand-dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun durand-open-terminal ()
  "Open terminal at the current directory."
  (interactive)
  (make-process :name "terminal" :command
                `("open" "-a" "terminal" ,(file-relative-name default-directory))
                :buffer nil))

;;;###autoload
(defun durand-open-dashboard ()
  "Open the dash board buffer."
  (interactive)
  (switch-to-buffer (doom-fallback-buffer)))

;;;###autoload
(defun durand-open-discord (&optional arg)
  "Open Discord.
With ARG \\[universal-argument], close discord."
  (interactive "P")
  (let ((browsing-command (cond ((equal arg '(4))
                                 '("osascript" "-e" "tell application \"Discord\" to quit"))
                                (t
                                 '("open" "-a" "Discord")))))
    (make-process
     :name "Discord"
     :command browsing-command
     :buffer nil))
  (when (equal arg '(4))
    (message "Discord closed.")))
