;;; ui/durand-modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar durand-buffer-name-max 50)

;;;###autoload
(defun doom-modeline-segment--buffer-info-durand ()
  "Almost the same as `doom-modeline-segment--buffer-info',
but it truncates the buffer name within `durand-buffer-name-max'."
  (concat
   ;; (doom-modeline--buffer-narrow-icon-durand)
   (s-truncate durand-buffer-name-max
               (format-mode-line (doom-modeline-segment--buffer-info))
               "...")))

(byte-compile 'doom-modeline-segment--buffer-info-durand)

;;;###autoload
(defvar doom-modeline--buffer-narrow-icon nil
  "Icon for the narrowing state of the buffer.")

;; NOTE: In doom-escape-hook, it stops at the first function that returns
;; non-nil. So I have to wrap it around.

;;;###autoload
(defun durand-update-buffer-file-state-icon (&rest _)
  "Wrapper around `doom-modeline-update-buffer-file-state-icon'"
  (doom-modeline-update-buffer-file-state-icon)
  nil)

(after! doom-modeline
;;;###autoload
  (defun doom-modeline-update-buffer-file-state-icon (&rest _)
    "Update the buffer or file state in mode-line. Modified by Durand."
    (setq doom-modeline--buffer-file-state-icon
          (when doom-modeline-buffer-state-icon
            (ignore-errors
              (concat
               (ignore-errors
                 (cond ((buffer-narrowed-p)
                        (doom-modeline-buffer-file-state-icon
                         "vertical_align_center" "↕" "><" 'doom-modeline-warning))
                       (t "")))
               (cond (buffer-read-only
                      (doom-modeline-buffer-file-state-icon
                       "lock" "🔒" "%1*" `(:inherit doom-modeline-warning
                                                    :weight ,(if doom-modeline-icon
                                                                 'normal
                                                               'bold))))
                     ((and buffer-file-name (buffer-modified-p)
                           doom-modeline-buffer-modification-icon)
                      (doom-modeline-buffer-file-state-icon
                       "save" "💾" "%1*" `(:inherit doom-modeline-buffer-modified
                                                    :weight ,(if doom-modeline-icon
                                                                 'normal
                                                               'bold))))
                     ((and buffer-file-name
                           (not (file-exists-p buffer-file-name)))
                      (doom-modeline-buffer-file-state-icon
                       "block" "🚫" "!" 'doom-modeline-urgent))
                     (t "")))))))
;;;###autoload
  (defsubst doom-modeline--buffer-narrow-icon-durand (&rest _arg)
    "The icon of the current narrowing state."
    (when doom-modeline-buffer-state-icon
      (when-let ((icon (or doom-modeline--buffer-narrow-icon
                           (doom-modeline-update-buffer-file-state-icon))))
        (concat
         (if (doom-modeline--active)
             icon
           (propertize icon 'face `(:inherit ,(get-text-property 0 'face icon)
                                             :inherit mode-line-inactive)))
         (doom-modeline-vspc)))))

;;;###autoload
  (defun doom-modeline-segment--buffer-position-durand ()
    "Almost the same as `doom-modeline-segment--buffer-position',
except when in `org-agenda-mode' it uses `org-agenda-show-blocks-number' instead."
    (cond
     ((derived-mode-p 'org-agenda-mode)
      (let* ((active (doom-modeline--active))
             (po (org-agenda-show-blocks-number))
             (face (if active 'mode-line 'mode-line-inactive))
             (mouse-face 'mode-line-highlight))
        (concat
         (doom-modeline-spc)
         (doom-modeline-spc)
         (propertize po
                     'face face
                     'help-echo "org agenda block position"
                     'mouse-face mouse-face))))
     (t
      (doom-modeline-segment--buffer-position)))))


(byte-compile 'doom-modeline-segment--buffer-position-durand)

;;;###autoload
(defun set-nil-mode-line ()
  "Disable mode line."
  (interactive)
  (setf mode-line-format nil)
  (force-mode-line-update))
