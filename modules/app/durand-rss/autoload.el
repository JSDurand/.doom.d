;;; app/durand-rss/autoload.el -*- lexical-binding: t; -*-

;; play youtube video
;;;###autoload
(defun durand-play-with-mpv (quality url)
  " Play a given URL with mpv."
  (interactive (list (durand-get-quality-val)
                     (read-string "Enter URL: ")))
  (let ((quality-arg "")
        (quality-val quality)
        (fit-arg "--autofit=100%x100%"))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height ≤ %s with mpv..."
             url
             quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]"
                                quality-val)))
    (eshell)
    (insert (format "mpv --no-terminal %s %s %s &"
                    quality-arg
                    fit-arg
                    url))
    (eshell-send-input)))

;;;###autoload
(defun durand-play-with-mpv-in-elfeed (quality)
  "If currently visiting a youtube feed entry or if the cursor is on a youtube feed entry,
then play the video with mpv with QUALITY, else just inform this is not a youtube link."
  (interactive (list (durand-get-quality-val)))
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected t)))
        (quality-arg "")
        (quality-val quality)
        (fit-arg "--autofit=100%x100%"))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height ≤ %s with mpv..."
             (elfeed-entry-link entry)
             quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]"
                                quality-val)))
    ;; (start-process "elfeed-mpv" nil "mpv" quality-arg fit-arg (elfeed-entry-link entry))
    (eshell)
    (insert (format "mpv --no-terminal %s %s %s &"
                    quality-arg
                    fit-arg
                    (elfeed-entry-link entry)))
    (eshell-send-input)))

;;;###autoload
(defvar elfeed-mpv-patterns
  '("youtu\\.?be")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.
Default value is \"youtu\\.?be\"")

;;;###autoload
(defun elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns'; visit it otherwise.
See `durand-play-with-mpv' also."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected t)))
        (patterns elfeed-mpv-patterns))
    (while (and patterns
                (not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (call-interactively 'durand-play-with-mpv-in-elfeed)
      (if (eq major-mode 'elfeed-search-mode)
          (let ((entries (elfeed-search-selected)))
            (cl-loop for entry in entries
                     when (elfeed-entry-link entry)
                     do (shell-command
                         (format
                          "open -a \"Safari\" --background %s" it)))
            (unless (or elfeed-search-remain-on-entry (use-region-p))
              (forward-line)))
        (elfeed-show-visit)))))

;;;###autoload
(defun durand-download-youtube (url &optional title)
  "Download the URL with youtube-dl.
URL can be a list of space-separated urls."
  (interactive (list (read-string "URL of video: ")))
  (let ((current-prefix-arg '(4)))
    (call-interactively 'eshell))
  (insert "cd ~/Desktop/Centre/Vidéos")
  (eshell-send-input)
  (let* ((urls (mapcar
                (lambda (s) (format "\"%s\"" s))
                (cl-remove-if
                 (lambda (s)
                   (= (length s) 0))
                 (split-string url " "))))
         (url (string-join urls " ")))
    (insert (if (not title)
                (format "youtube-dl %s" url)
              (format "youtube-dl %s -o \"%s.mp4\"" url title))))
  (eshell-send-input)
  (if title
      (message "Downloading %s as %s.mp4" url title)
    (message "Downloading %s" url)))

;;;###autoload
(defun elfeed-download-youtube ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns'; do nothing otherwise."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected t)))
         (link (elfeed-entry-link entry))
         (title (elfeed-entry-title entry))
         (patterns elfeed-mpv-patterns))
    (while (and patterns
                (not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (when patterns
      (durand-download-youtube link title))))

;;;###autoload
(defun durand-get-quality-val ()
  "Let the user choose a quality format."
  (completing-read
   "Max height resolution (0 for unlimited): "
   '("0" "480" "720")))

;;;###autoload
(defmacro make-tag-toggler (arg)
  "Make a function that toggles the tag ARG on or off in elfeed search"
  `(defun ,(intern (format "%s-tag-toggler" arg)) ()
     ,(format "Toggle %s tag" arg)
     (interactive)
     (cond ((string-match (concat " \\+" ,arg) elfeed-search-filter)
            (elfeed-search-set-filter (replace-match "" nil nil elfeed-search-filter)))
           ((string-match (concat " -" ,arg) elfeed-search-filter)
            (elfeed-search-set-filter (replace-match (concat " +" ,arg) nil nil elfeed-search-filter)))
           (t
            (elfeed-search-set-filter (concat elfeed-search-filter (concat " -" ,arg)))))
     (message elfeed-search-filter)))

;;;###autoload
(defmacro make-toggler (arg)
  "Make a function that toggles ARG on or off in elfeed search"
  `(defun ,(intern (format "%s-toggler" arg)) ()
     ,(format "Toggle %s" arg)
     (interactive)
     (cond ((string-match (concat " " ,arg) elfeed-search-filter)
            (elfeed-search-set-filter (replace-match "" nil nil elfeed-search-filter)))
           (t
            (elfeed-search-set-filter (concat elfeed-search-filter (concat " " ,arg)))))
     (message elfeed-search-filter)))

;;;###autoload
(make-tag-toggler "youtube")

;;;###autoload
(make-tag-toggler "emacs")

;;;###autoload
(make-tag-toggler "blog")

;;;###autoload
(make-tag-toggler "relevant")

;;;###autoload
(make-tag-toggler "important")

;;;###autoload
(make-tag-toggler "unread")

;;;###autoload
(make-tag-toggler "haskell")

;;;###autoload
(make-toggler "math")

;;;###autoload
(defun elfeed-next-entry (&optional n)
  "Move to the next entry in search buffer"
  (interactive "p")
  (forward-line n))

;;;###autoload
(defun elfeed-previous-entry (&optional n)
  "Move to the previous entry in search buffer"
  (interactive "p")
  (forward-line (- n)))

;;;###autoload
(defun durand-elfeed-filter-complete ()
  "Complete filters for elfeed."
  (interactive)
  ;; NOTE: Make sure space is not bound
  (require 'crm)
  (define-key crm-local-completion-map [32] nil)
  (let* ((elfeed-search-filter-active :live)
         (crm-separator " ")
         (tags (elfeed-db-get-all-tags))
         (tags (cl-loop for tag in tags
                        append (list
                                (format "+%s" tag)
                                (format "-%s" tag))))
         (filters (completing-read-multiple
                   "Filter: "
                   tags nil nil
                   elfeed-search-filter)))
    (ignore crm-separator elfeed-search-filter-active)
    (setf elfeed-search-filter
          (string-join filters " "))
    (elfeed-search-update :force)))
