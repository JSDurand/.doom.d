;;; config/durand-ideal/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar durand-stop-timer nil "The timer to control stopping to take a rest!")

;;;###autoload
(defvar durand-back-timer nil "The timer to control going back to work!")

;; show current time

;;;###autoload
(defun durand-show-current-time ()
  "Show the current time.
Invoke the command again to disable it.
With prefix arg, show in a separate window."
  (interactive)
  (let ((time-string (format-time-string "%A %e %B %H:%M:%S")))
    (if current-prefix-arg
        (with-current-buffer-window
         "*current time*" nil nil
         (prin1 time-string)
         (helpful-mode))
      ;; (message time-string)
      (display-time-mode (cond
                          (display-time-mode
                           -1)
                          (t
                           1))))))
