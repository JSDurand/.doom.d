;;; completion/durand-ivy/autoload.el -*- lexical-binding: t; -*-

;; format function

;;;###autoload
(defface durand-arrow-face
  '((t
     (:inherit minibuffer-prompt :foreground "gold" :height 250)))
  "Face for the arrow used by `durand-ivy-format-function-arrow'.")

;;;###autoload
(defun durand-ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer using an icon instead of \">\"."
  (ivy--format-function-generic
   (lambda (str)
     (concat
      (propertize
       (format "%s "
               (all-the-icons-material "school" :face 'durand-arrow-face)))
      (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "   " str))
   cands
   "\n"))

;;;###autoload
(defun durand-ivy-rich-bookmark-file-path (file)
  "Show the file path of the bookmark item FILE."
  (bookmark-location file))

;;;###autoload
(defface durand-buffer-major-mode-face
  '((default . (:foreground "light blue")))
  "Face for major mode info in `durand-switch-buffer' function")


;;;###autoload
(defun ivy-file-name-extension (file)
  "Show the extension of the file name.
     This is always a string."
  (cond ((stringp (file-name-extension file))
         (file-name-extension file))
        ((and (null (file-name-extension file))
              (file-directory-p file))
         "dir")
        (t "")))

;;;###autoload
(defun durand-find-file-go-to-dir-action (path)
  "Go to the directory containing PATH."
  (interactive)
  (let ((dir (expand-file-name
              (file-name-directory path)
              (when-let ((root (doom-project-root)))
                root))))
    (cl-assert (file-directory-p dir))
    (dired dir)))

;;; Archive
;; (propertize "☸ "
;;             'face
;;             'durand-arrow-face)
