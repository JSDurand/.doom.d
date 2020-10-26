;;; ui/durand-modeline/autoload.el -*- lexical-binding: t; -*-
;;;###if (not (featurep! :ui modeline +light))

;;;###autoload
(defvar durand-buffer-name-max 50
  "The maximal length of the buffer name in modeline.")

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
(defun durand-doom-modeline--font-height-a ()
  "Advice to `doom-modeline--font-height'."
  (+ (frame-char-height) 3))

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
                        (concat
                         (propertize
                          (doom-modeline-buffer-file-state-icon
                           "vertical_align_center" "↕" "><" 'doom-modeline-warning)
                          'help-echo "Narrowed")
                         (doom-modeline-spc)))
                       (t "")))
               (cond (buffer-read-only
                      (propertize
                       (doom-modeline-buffer-file-state-icon
                       "lock" "🔒" "%1*" `(:inherit doom-modeline-warning
                                                    :weight ,(if doom-modeline-icon
                                                                 'normal
                                                               'bold)))
                       'help-echo "Read only"))
                     ((and buffer-file-name (buffer-modified-p)
                           doom-modeline-buffer-modification-icon)
                      (propertize
                       (doom-modeline-buffer-file-state-icon
                       "save" "💾" "%1*" `(:inherit doom-modeline-buffer-modified
                                                    :weight ,(if doom-modeline-icon
                                                                 'normal
                                                               'bold)))
                       'help-echo "Modified"))
                     ((and buffer-file-name
                           (not (file-exists-p buffer-file-name)))
                      (propertize
                       (doom-modeline-buffer-file-state-icon
                        "block" "🚫" "!" 'doom-modeline-urgent)
                       'help-echo "File not saved yet"))
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
     ((derived-mode-p 'durand-agenda-mode)
      (let* ((active (doom-modeline--active))
             (po (durand-agenda-section-indicator))
             (face (if active 'mode-line 'mode-line-inactive))
             (mouse-face 'mode-line-highlight))
        (concat
         (doom-modeline-spc)
         (doom-modeline-spc)
         (propertize po
                     'face face
                     'help-echo "org agenda block position"
                     'mouse-face mouse-face))))
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

;;; showing org-agenda tasks on the mode-line

;;;###autoload
(defvar durand-agenda-mode-line nil
  "The indicator on the mode-line.
This is supposed to show how many items are there for today.")

;;;###autoload
(defun oaam-get-entries-for-date (&optional date include-archive-p files)
  "Collect entries in FILES for DATE.
If INCLUDE-ARCHIVE-P is non-nil, then archived entries are also included, else
they are not counted.
If DATE is nil, then it defaults to today as returned by
`calendar-current-date'.
If FILES is nil, then it defaults to `org-agenda-files'."
  (let* ((date (or date (calendar-current-date)))
         (files (or files org-agenda-files))
         (org-agenda-buffer nil)
         (raw-entries (cl-loop for file in files
                               append (org-agenda-get-day-entries
                                       file date)))
         (filtered (cl-loop for entry in raw-entries
                            when (cond
                                  (include-archive-p t)
                                  (t (not
                                      (cl-member
                                       "archive"
                                       (get-text-property 0 'tags entry)
                                       :test 'string=))))
                            collect (list
                                     (substring-no-properties entry)
                                     (get-text-property 0 'org-hd-marker entry)))))
    (ignore org-agenda-buffer nil)
    filtered))

;;;###autoload
(defun doom-modeline-segment--org-agenda ()
  "Display the number of org-agenda tasks on the mode-line.
Specifically this depends on the variable
`durand-agenda-mode-line' to show on the mode-line. And this
variable is updated after you call `durand-agenda', and every
morning at 9:00AM."
  (cond
   ((and durand-agenda-mode-line
         (numberp durand-agenda-mode-line)
         (> durand-agenda-mode-line 0))
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap [mode-line mouse-1] 'durand-agenda)
      (propertize
       (format "%s %d"
               (doom-modeline-icon 'faicon "tasks" "📝" "T" 'doom-modeline-warning)
               durand-agenda-mode-line)
       'mouse-face 'mode-line-highlight
       'help-echo "Number of tasks to do.\nClick to call `durand-agenda'."
       'local-map keymap)))
   (t "")))

(byte-compile 'doom-modeline-segment--org-agenda)

;;;###autoload
;; (defadvice! durand-update-agenda-mode-line (&rest _args)
;;   "Update `durand-agenda-mode-line'."
;;   :before 'org-agenda-exit
;;   (setf durand-agenda-mode-line
;;         (length (oaam-get-entries-for-date))))
