;;; config/durand-ideal/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar durand-stop-timer nil "The timer to control stopping to take a rest!")

;;;###autoload
(defvar durand-back-timer nil "The timer to control going back to work!")

;; show current time

;;;###autoload
(defun durand-show-current-time (&optional arg)
  "Show the current time.
Invoke the command again to disable it.
When ARG is non-nil, show it in a pop-up window."
  (interactive "P")
  (let ((time-string (format-time-string "%A %e %B %H:%M:%S"))
        (display-time-day-and-date t))
    (if arg
        (with-current-buffer-window
         "*current time*" nil nil
         (prin1 time-string)
         (helpful-mode))
      ;; (message time-string)
      (display-time-mode (cond
                          (display-time-mode -1)
                          (t 1))))))

;;;###autoload
(defvar durand-jump-hook-alist nil
  "An alist to store actions that should be performed after `durand-bookmark-jump-headlong' is called")

;; some actions
(setf durand-jump-hook-alist
      '(("account.org" . (lambda ()
                           (org-account-go-to-last-day t)
                           (durand-show-account-report)
                           (let ((mes (substitute-command-keys "\\<account-report-mode-map>Press \\[durand-view-last-day] to view the last day; \\[durand-view-last-week] to view the last week; \\[durand-view-last-month] to view the last month; \\[durand-view-last-year] to view the last year;
\\[durand-view-last-custom] to specify a custom continuous range."))) (message mes))
                           ;; (org-columns)
                           ))
        ;; ("account.ledger" . (lambda ()
        ;;                       (goto-char (point-max))))
        ;; (".*tex$" . durand-tex-action)
        ;; ("ideal.el" . (lambda ()
        ;;                 (ignore-errors
        ;;                   (goto-char (point-min))
        ;;                   (search-forward "defhydra" nil 'go 2)
        ;;                   (forward-line 0)
        ;;                   (recenter 0))))
        ("\\(diary\\|notes\\).org" . (lambda ()
                                       (goto-char (point-max))
                                       (org-show-context)
                                       (recenter -3)))))

;;;###autoload
(defvar durand-jumped nil
  "Determine if `durand-bookmark-jump-headlong' executes the hooks at the end")

;;;###autoload
(defun durand-bookmark-jump-headlong ()
  "Inspired by `bookmark-jump-headlong', but using `ivy-read':
If there is only one match, then perform the default action"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (reset-durand-changed)
  (reset-durand-headlong)
  (unwind-protect
      (ivy-read "Jump to bookmark: " (mapcar #'car bookmark-alist)
                :action '(1
                          ("o" (lambda (x)
                                 (interactive)
                                 (bookmark-jump (assoc x bookmark-alist))
                                 (setf bookmark-current-bookmark x))
                           "jump")
                          ("k" (lambda (x)
                                 (interactive)
                                 (setq bookmark-alist (delete (assoc x bookmark-alist) bookmark-alist))
                                 (setf (ivy-state-collection ivy-last)
                                       (mapcar #'car bookmark-alist))
                                 (setf ivy--index (max 0 (1- ivy--index)))
                                 (ivy--reset-state ivy-last))
                           "kill"))
                :update-fn 'durand-self-insert-complete-and-exit
                :initial-input "^"
                :re-builder 'ivy--regex-plus
                :unwind 'reset-durand-changed
                :keymap durand-switch-buffer-map
                :caller 'durand-bookmark-jump-headlong)
    (+workspaces-add-current-buffer-h)
    (let* ((cur-name (buffer-name))
           (action (assoc-default cur-name durand-jump-hook-alist
                                  (lambda (element key)
                                    (string-match element key)))))
      (cond
       ((not (null action))
        (funcall action))
       (t nil)))))

;;;###autoload
(defun durand-recentf-jump-headlong (&optional arg)
  "Jump to recent files.
If there is only one match, then perform the default action"
  (interactive "P")
  (durand-update-recentf)
  (reset-durand-changed)
  (reset-durand-headlong)
  (if arg
      (if durand-recently-closed-files
          (find-file (pop durand-recently-closed-files))
        (user-error "No recently closed files"))
    (ivy-read "Recent files: " (mapcar #'car durand-recentf-list)
              :action '(1
                        ("o" (lambda (x)
                               (interactive)
                               (find-file (cdr (assoc x durand-recentf-list))))
                         "Open the file")
                        ("k" (lambda (x)
                               (interactive)
                               (durand-kill-from-recentf x)
                               (durand-update-recentf)
                               (setf (ivy-state-collection ivy-last)
                                     (mapcar #'car durand-recentf-list))
                               (setf ivy--index (max 0 (1- ivy--index)))
                               (ivy--reset-state ivy-last))
                         "Kill it from RECENTF-LIST"))
              :update-fn 'durand-self-insert-complete-and-exit
              :initial-input "^"
              :re-builder 'ivy--regex-plus
              :unwind 'reset-durand-changed
              :keymap durand-switch-buffer-map
              :caller 'durand-recentf-jump-headlong)))

;;;###autoload
(defun durand-self-insert-complete-and-exit (&rest _args)
  "Taken from headlong file.
Insert the character you type and try to complete.
If this results in one candidate, then immediately exit the minibuffer with the default action."
  (interactive)
  (when durand-headlong
    (let* ((cands (let ((collection (ivy-state-collection ivy-last)))
                    (cond
                     ((sequencep collection)
                      (ivy--filter ivy-text collection))
                     ((functionp collection)
                      (funcall collection ivy-text)))))
           (candidates-length (safe-length cands)))
      (cond
       ((= 1 candidates-length)
        (if durand-changed-p
            (ivy-done)
          (setq durand-changed-p t)))
       (t (setq durand-changed-p t))))))

;;;###autoload
(defvar durand-headlong t
  "Whether to use headlong mode or not.
Default to t")

;;;###autoload
(defun reset-durand-headlong ()
  "Reset DURAND-HEADLONG"
  (setq durand-headlong t))

;;;###autoload
(defun durand-toggle-headlong ()
  "Toggle DURAND-HEADLONG"
  (interactive)
  (setq durand-headlong (not durand-headlong)))

;;;###autoload
(defvar durand-changed-p nil
  "A variable to determine if `ivy-read' is just initialised.")

;;;###autoload
(defun reset-durand-changed ()
  "Reset the variable DURAND-CHANGED-P to nil"
  (setq durand-changed-p nil))

;; update durand-recentf
;;;###autoload
(defun durand-update-recentf ()
  "Simple `mapcar'"
  (interactive)
  (setq durand-recentf-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list)))

;;;###autoload
(defalias 'quit-other-window 'durand-quit-other-window)

;;;###autoload
(defun durand-quit-other-window ()
  "Quit the other window."
  (interactive)
  (other-window 1)
  (quit-window))

;;;###autoload
(defun durand-quit-window ()
  "Quit the window."
  (interactive)
  (quit-window))

;;;###autoload
(defun durand-switch-buffer (&optional _buf)
  "Use `durand-complete-buffer' to choose buffers to switch to.
See the documentation of `durand-complete-buffer' to know more."
  (interactive)
  (reset-durand-headlong)
  (ivy-read "buffer: " 'durand-complete-buffer
            :dynamic-collection t
            :action '(1
                      ("o" switch-to-buffer
                       "Switch to buffer")
                      ("k" (lambda (x)
                             (interactive)
                             (and (get-buffer x)
                                  (kill-buffer x))
                             (ivy--reset-state ivy-last))
                       "Kill"))
            :update-fn 'durand-self-insert-complete-and-exit
            :re-builder (if current-prefix-arg 'ivy--regex-ignore-order 'ivy--regex-fuzzy)
            :initial-input (cond ((equal current-prefix-arg '(4)) "^*")
                                 ((equal current-prefix-arg '(16)) " ")
                                 (t "^"))
            :unwind 'reset-durand-changed
            :caller 'ivy-switch-buffer
            :keymap 'durand-switch-buffer-map))

;;;###autoload
(defun durand-complete-buffer (str &rest _args)
  "Complete buffers; intended to be used as `collection' of `ivy-read' with `dynamic-collection' set to t.
If STR starts with \"-\", then complete also with mode name;
if STR starts with \"/\", then complete also with the file name;
if STR starts with a space, then consider also hidden buffers."
  (cl-loop for buffer being the buffers
           when (let* ((nom (buffer-name buffer))
                       (re-str (funcall (or (ivy-state-re-builder ivy-last)
                                            'ivy--regex-fuzzy)
                                        str))
                       (matcher (if (stringp re-str) 'string-match 'ivy-re-match)))
                  (cond
                   ((and (> (length str) 0)
                         (= (aref str 0) 32)
                         (= (aref nom 0) 32))
                    (funcall matcher re-str nom))
                   ((= (aref nom 0) 32) nil)
                   ((and (> (length str) 0)
                         (= (aref str 0) ?-))
                    (let* ((re-str
                            (funcall (or (ivy-state-re-builder ivy-last)
                                         'ivy--regex-fuzzy)
                                     (substring str 1)))
                           (matcher (if (stringp re-str) 'string-match 'ivy-re-match)))
                      (or (funcall matcher re-str nom)
                          (funcall matcher re-str
                                   (ivy-rich-switch-buffer-major-mode
                                    nom)))))
                   ((and (> (length str) 0)
                         (= (aref str 0) ?/))
                    (let* ((re-str
                            (funcall (or (ivy-state-re-builder ivy-last)
                                         'ivy--regex-fuzzy)
                                     (substring str 1)))
                           (matcher (if (stringp re-str) 'string-match 'ivy-re-match)))
                      (or (funcall matcher re-str nom)
                          (funcall matcher re-str
                                   (ivy-rich-switch-buffer-path nom)))))
                   (t (funcall matcher re-str nom))))
           when (cond ((= (aref (buffer-name buffer) 0) 32) t)
                      ((and (string-prefix-p "*" (buffer-name buffer))
                            (string-prefix-p "^*" str))
                       t)
                      ((featurep! :ui workspaces)
                       (+workspace-contains-buffer-p buffer))
                      (t t))
           collect (buffer-name buffer)))

;;;###autoload
(defun durand-other-buffer ()
  "Switch to the other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun durand-new-buffer (&optional name)
  "Create a new buffer with name NAME; NAME defaults to *sans titre*"
  (interactive)
  (let ((name (or name
                  (let ((n 1))
                    (while (get-buffer (format "sans titre<%d>" n))
                      (incf n))
                    (format "sans titre<%d>" n)))))
    (switch-to-buffer name)))

;;;###autoload
(defun durand-beginning-of-line-or-block (&optional arg)
  "Adapted from `xah-beginning-of-line-or-block'.
Move cursor to the beginning of line or the previous block.
When cursor is not on the beginning of visual line, then move to the beginning of visual line.
When cursor is on the beginning of visual line, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
With prefix arg jump between pages.
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04"
  (interactive "P")
  (let ((regex (if arg "\n\f" "\n[\t\n ]*\n+")))
    (cond
     ((and arg (equal arg '(4)))
      (re-search-backward regex nil 'go))
     ((equal (point) (save-excursion (beginning-of-visual-line) (point)))
      (if (not (re-search-backward regex nil 'go))
          (goto-char (point-min))
        (goto-char (match-beginning 0))
        (forward-char 1)))
     (t
      (beginning-of-visual-line)))))

;; REVIEW: This is not needed, but still kept around.
;;;###autoload
;; (defun durand-end-of-line-or-block (&optional arg)
;;   "From `xah-end-of-line-or-block'.
;; Move cursor to end of line or next paragraph.
;; When called the first time, move to end of line
;; When called more than once, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
;; URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
;; Version 2018-06-04"
;;   (interactive "P")
;;   (let (($p (point))
;;         (regex (if current-prefix-arg "\n\f" "\n[\t\n ]*\n+")))
;;     (cond
;;      ((and current-prefix-arg (equal current-prefix-arg '(4)))
;;       (re-search-forward regex nil 'go))
;;      ((memq (point) (save-excursion (end-of-visual-line) (list (1- (point)) (point))))
;;       (re-search-forward regex nil 'go)
;;       (backward-char 1))
;;      (t
;;       (end-of-visual-line)))))

;;;###autoload
(defun forward-or-up-sexp ()
  "forward or up sexp"
  (interactive)
  (condition-case nil
      (forward-sexp 1)
    (scan-error (up-list))))

;;;###autoload
(defun backward-or-up-sexp ()
  "backward or up sexp"
  (interactive)
  (condition-case nil
      (backward-sexp 1)
    (scan-error (backward-up-list))))

;;;###autoload
(defun durand-general-save-buffer ()
  "Save buffer or org-agenda-save"
  (interactive)
  (pcase major-mode
    ((guard (derived-mode-p 'org-agenda-mode))
     (org-save-all-org-buffers))
    ;; ((guard (derived-mode-p 'tex-mode))
    ;;  (save-buffer 0)
    ;;  (setf durand-tex-action (lambda ()
    ;;                            "tex or display or kill"
    ;;                            (interactive)
    ;;                            (if current-prefix-arg
    ;;                                (tex-display-or-kill-temp-buffer)
    ;;                              (tex)))))
    (_
     (save-buffer 0))))

;; REVIEW: Only for some edge cases.
;;;###autoload
(defun durand-buffer-scroll (&optional direction n other-p)
  "Scroll buffer"
  (interactive)
  (let* ((direction (or direction 'up))
         (old-n n)
         (n (or n (/ (window-body-height) 2)))
         (scroll-function (pcase direction
                            ('up '(
                                   :normal scroll-up
                                   :other-window durand-other-window-scroll
                                   :pdf pdf-view-scroll-up-or-next-page
                                   :agenda org-agenda-next-block))
                            ('down '(
                                     :normal scroll-down
                                     :other-window durand-other-window-scroll
                                     :pdf pdf-view-scroll-down-or-previous-page
                                     :agenda org-agenda-previous-block))
                            (_ (user-error "Invalid direction %s" direction)))))
    (condition-case err
        (cond
         ((and other-p (null (one-window-p)))
          (funcall (plist-get scroll-function :other-window) direction old-n))
         ((derived-mode-p 'novel-mode)
          (funcall (plist-get scroll-function :normal) 10))
         ((string= major-mode "pdf-view-mode")
          (funcall (plist-get scroll-function :pdf) n))
         (other-p
          (funcall (plist-get scroll-function :normal) n))
         ((string= major-mode "org-agenda-mode")
          (funcall (plist-get scroll-function :agenda) (or n 1)))
         (t
          (funcall (plist-get scroll-function :normal) n)))
      ((user-error error) (pcase (cdr err)
                            ((pred null)
                             (message "%s" (car err)))
                            (_
                             (message "%s: %s" (car err) (cdr err))))))))

;;;###autoload
(defun durand-other-window-scroll (&optional direction n)
  "Scroll the buffer in the other window"
  (let* ((direction (or direction 'up))
         (n (or n (/ (window-body-height) 2)))
         (scroll-function (pcase direction
                            ('up '(
                                   :normal scroll-up
                                   :pdf pdf-view-scroll-up-or-next-page
                                   :agenda org-agenda-next-block))
                            ('down '(
                                     :normal scroll-down
                                     :pdf pdf-view-scroll-down-or-previous-page
                                     :agenda org-agenda-previous-block))
                            (_ (user-error "Invalid direction %s" direction)))))
    (other-window 1)
    (unwind-protect
        (condition-case err
            (cond
             ((string= major-mode "pdf-view-mode")
              (funcall (plist-get scroll-function :pdf) n))
             ((string= major-mode "org-agenda-mode")
              (funcall (plist-get scroll-function :agenda) (or n 1)))
             (t
              (funcall (plist-get scroll-function :normal) n)))
          ((user-error error) (pcase (cdr err)
                                ((pred null)
                                 (message "%s" (car err)))
                                (_
                                 (message "%s: %s" (car err) (cdr err))))))
      (other-window -1))))

;; Rechercher les fichiers pdf
;;;###autoload
(defvar pdf-dir-list
  '("/Users/durand/Downloads/"
    "/Users/durand/Desktop/Centre/Documents partout"
    "/Users/durand/Desktop/Centre/Je veux lire/"
    "/Users/durand/Desktop/Centre/LaTeX temporaire/"
    "/Users/durand/Desktop/Centre/MaoBaoBao/Autres/PDF/"
    "/Users/durand/Desktop/Centre/MaoBaoBao/Mao Problems/"
    "/Users/durand/Desktop/Centre/Mes notes/"
    "/Users/durand/Desktop/Centre/PDF/"
    "/Users/durand/Desktop/Centre/Pour thèse/"
    "/Users/durand/Desktop/Centre/TeX/"
    "/Users/durand/Desktop/Centre/Œuvres de professeur/"
    "/Users/durand/Desktop/Centre/方便與智慧無二/"
    "/Users/durand/Desktop/Centre/Échecs")
  "La liste des dossiers où je peux chercher les fichiers pdf quand j'ai besoin.")

;;;###autoload
(defface durand-pdf-dir-face '((t :foreground "orange2"))
  "Face for directory in durand-pdf-mode")

;;;###autoload
(defface durand-pdf-nom-face '((t :foreground "SkyBlue1"))
  "Face for file name in durand-pdf-mode")

;;;###autoload
(defun durand-modify-pdf-buffer ()
  "Changer le tampon"
  (goto-char (point-min))
  (save-excursion
    (while (/= (point) (point-max))
      (put-text-property (point) (line-end-position) 'face 'durand-pdf-nom-face)
      (forward-line)))
  (let* ((chemin (buffer-substring-no-properties
                  (point) (line-end-position)))
         (dossier (file-name-directory chemin))
         (nom (file-name-nondirectory chemin))
         (inhibit-read-only t))
    (insert (propertize (concat dossier "\n")
                        'face 'durand-pdf-dir-face
                        'chemin dossier))
    (put-text-property (point) (line-end-position) 'display nom)
    (put-text-property (point) (line-end-position) 'chemin chemin)
    (save-excursion
      (while (/= (point) (point-max))
        (let* ((nouveau-chemin (buffer-substring-no-properties
                                (point) (line-end-position)))
               (nouveau-dossier (file-name-directory nouveau-chemin))
               (nouveau-nom (file-name-nondirectory nouveau-chemin)))
          (when (not (string-equal nouveau-dossier dossier))
            (setf dossier nouveau-dossier)
            (insert (propertize (concat "\n" nouveau-dossier "\n")
                                'face 'durand-pdf-dir-face
                                'chemin nouveau-dossier)))
          (put-text-property (point) (line-end-position) 'display nouveau-nom)
          (put-text-property (point) (line-end-position) 'chemin nouveau-chemin)
          (forward-line)))))
  (goto-char (point-min)))

;;;###autoload
(define-derived-mode durand-pdf-mode special-mode "Durand PDF"
  "chercher pdf"
  (face-remap-add-relative 'default '(:foreground "orange2"))
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (save-excursion
      (while (re-search-forward "\\([^p]..$\\|p[^d].$\\|pd[^f]$\\)" nil t)
        (delete-region (max (1- (line-beginning-position))
                            (point-min))
                       (line-end-position))))
    (durand-modify-pdf-buffer)))

;;;###autoload
(defun durand-pdf-open-pdf ()
  "Open pdf or directory under point"
  (interactive)
  (let ((inhibit-read-only t)
        (chemin (get-text-property (point) 'chemin)))
    (if (and chemin (file-readable-p chemin))
        (find-file chemin)
      (message "Command not allowed on this line"))))

;;;###autoload
(defun durand-pdf-open-or-scroll-up ()
  "Open pdf or directory under point in another window"
  (interactive)
  (let* ((inhibit-read-only t)
         (chemin (get-text-property (point) 'chemin))
         (nom (file-name-nondirectory chemin)))
    (cond
     ((get-buffer nom)
      (save-selected-window
        (other-window 1)
        (pdf-view-scroll-up-or-next-page)))
     ((and chemin (file-readable-p chemin))
      (save-selected-window
        (find-file-other-window chemin)))
     (t
      (message "Command not allowed on this line")))))

;;;###autoload
(defun durand-pdf-open-or-scroll-down ()
  "Scroll down pdf"
  (interactive)
  (let* ((inhibit-read-only t)
         (chemin (get-text-property (point) 'chemin))
         (nom (file-name-nondirectory chemin)))
    (cond
     ((get-buffer nom)
      (save-selected-window
        (other-window 1)
        (pdf-view-scroll-down-or-previous-page)))
     ((and chemin (file-readable-p chemin))
      (save-selected-window
        (find-file-other-window chemin)))
     (t
      (message "Command not allowed on this line")))))

;;;###autoload
(defun durand-pdf-next-pdf-line ()
  "Next PDF line"
  (interactive)
  (let ((orig (point)))
    (end-of-line)
    (if (re-search-forward ".pdf$" nil t)
        (beginning-of-line)
      (goto-char orig)
      (message "No next pdf line"))))

;;;###autoload
(defun durand-pdf-previous-pdf-line ()
  "Next PDF line"
  (interactive)
  (let ((orig (point)))
    (beginning-of-line)
    (if (re-search-forward ".pdf$" nil t -1)
        (beginning-of-line)
      (goto-char orig)
      (message "No previous pdf line"))))

;;;###autoload
(defun durand-chercher-pdf (nom)
  "Chercher les fichers pdf par NOM"
  (interactive (list (read-string "Chercher: ")))
  (let ((chercher-buffer "*chercher pdf*")
        (nom (concat "*" nom "*")))
    (when (get-buffer chercher-buffer)
      (kill-buffer chercher-buffer))
    (switch-to-buffer chercher-buffer)
    ;; (pop-to-buffer-same-window chercher-buffer)
    ;; (delete-other-windows)
    (delete-region (point-min) (point-max))
    (dolist (dir pdf-dir-list)
      (let ((pro (make-process
                  :name "chercher"
                  :buffer chercher-buffer
                  :sentinel 'ignore
                  :command `("rg" "--files" "--no-messages" "--follow" "--iglob" ,nom ,dir))))
        (accept-process-output pro)))
    (durand-pdf-mode))
  (message "Chercher %s" nom))

;;;###autoload
(defun capitalize-region-or-word ()
  "capitalize region or word"
  (interactive)
  (if (use-region-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word 1)))

(setf align-default-spacing 1)

;; opening links
;;;###autoload
(defun durand-open-link (&optional arg)
  "Open various types of links under cursor,
or forward to links if ARG is non-nil, or if the link is not found.
Also open message in `mu4e-headers-mode' without forwarding to links even if no link is found."
  (interactive "P")
  (if arg
      (durand-forward-link)
    (condition-case err
        (pcase major-mode
          ('org-mode
           (org-open-at-point-decoded))
          ('help-mode
           (when (equal (push-button) nil)
             (durand-forward-link)))
          ('mu4e-headers-mode
           (mu4e-headers-view-message))
          ('mu4e-main-mode
           (mu4e-headers-search-bookmark))
          ('dired-mode
           (dired-find-file))
          (_
           (browse-url (durand-find-next-link nil 'get-text-property))))
      ((error user-error)
       (durand-forward-link)
       (message "%s" err)))))

;;;###autoload
(defun durand-open-browser (&optional arg)
  "Open browser.
With ARG \\[universal-argument], prompt for some frequently visited websites.
With ARG \\[universal-argument]\\[universal-argument], close Safari."
  (interactive "P")
  (let* ((cands '(("google" . "https://google.com")
                  ("math" . "https://math.stackexchange.com/questions")
                  ("fb" . "https://facebook.com")
                  ("twitter" . "https://twitter.com")
                  ("ask" . "https://ask.fm")
                  ("YT" . "https://youtube.com")))
         (choix (when (equal arg '(4)) (ivy-read "Chois un lien: " cands)))
         (link (when choix
                 (cond
                  ((assoc-default choix cands)
                   (assoc-default choix cands))
                  ((not (string-prefix-p "http" choix))
                   (string-join `("https://" ,choix)))
                  (t
                   choix))))
         (browsing-command (cond ((equal arg '(16))
                                  '("osascript" "-e" "tell application \"Safari\" to quit"))
                                 (link
                                  `("open" "-a" "Safari" ,link))
                                 (t
                                  '("open" "-a" "Safari")))))
    (make-process
     :name "Safari"
     :command browsing-command
     :buffer nil))
  (when (equal arg '(16))
    (message "Safari closed.")))

;;;###autoload
(defvar durand-stop-music-map (let ((map (make-sparse-keymap)))
                                (define-key map [?\r]
                                  (lambda ()
                                    (interactive)
                                    (kill-process (get-process "mpv"))))
                                (define-key map [32]
                                  (lambda ()
                                    (interactive)
                                    (kill-process (get-process "mpv"))))
                                map)
  "A transient map used so that pressing return stops the music")

;;;###autoload
(defun durand-come-back ()
  "Tell Durand to come back!"
  (interactive)
  (when (timerp durand-back-timer)
    (cancel-timer durand-back-timer))
  (setf durand-back-timer nil)
  (set-transient-map durand-stop-music-map)
  (make-process
   :name "mpv"
   :buffer nil
   :command '("mpv"
              "/Users/durand/Desktop/Centre/Musique/Chansons/Pure/Marble Machine Cover.mp3")))

;;;###autoload
(defun durand-stop-reminder ()
  "Send a notification"
  (interactive)
  (when (timerp durand-back-timer)
    (cancel-timer durand-back-timer))
  (setf durand-back-timer (run-with-timer 1200 nil 'durand-come-back))
  (make-process
   :name "stop"
   :buffer nil
   :command ;; '("say" "Time is up!")
   '("terminal-notifier"
     "-title" "\"Stop!\""
     "-message" "\"Take a rest!\""
     "-execute" "\"say Il est venu le temps!\"")
   ))
;; "-execute" "\"pmset displaysleepnow\""

;;;###autoload
(defun durand-narrow-dwim (arg)
  "Widen when narrowed, unless ARG is non-nil.
Quit org-edit-src, unless ARG is non-nil.
When region is active, narrow to that region.
In org-mode, if ARG is '(16), then execute `org-edit-special';
else try `org-edit-src-code', `org-narrow-to-block',
`org-narrow-to-subtree', and `org-edit-special' in this order.
Otherwise execute `narrow-to-defun'."
  (interactive "P")
  (cond
   ((and (buffer-narrowed-p) (not arg)) (widen))
   ((and (string-prefix-p "*Org Src" (buffer-name))
         (not arg))
    (org-edit-src-exit))
   ((region-active-p)
    (narrow-to-region (region-beginning) (region-end)))
   ((derived-mode-p 'org-mode)
    (cond
     ((equal arg '(16))
      (let ((current-prefix-arg nil))
        (ignore-errors (org-edit-special nil))))
     ((ignore-errors (org-edit-src-code) t)
      (delete-other-windows))
     ((ignore-errors (org-narrow-to-block) t))
     ((ignore-errors (org-narrow-to-subtree) t))
     ((let ((current-prefix-arg nil))
        (ignore-errors (org-edit-special nil) t)))
     (t (message "No pre-defined behaviour."))))
   (t
    (narrow-to-defun))))

;; clean up buffers

;;;###autoload
(defun is-not-needed-buffer (buf &optional reg)
  "Match some buffers I do not want to keep around.
REG is additonal regexp to match as not needed."
  (let ((name (buffer-name buf)))
    (and
     (or (string= " *server*" name)
         (= ?* (aref name 0))
         (string-match "^magit" name)
         (string-match "^TAGS\\(<.*>\\)?$" name)
         (when (stringp reg)
           (string-match reg name)))
     (null (get-buffer-process name)))))

;;;###autoload
(defun clean-up-buffers (&optional arg)
  "Clean up some buffers that I oft do not need to keep around and kill unnecessary timers;
If the buffer has a running process, then do not kill it.
If \\[universal-argument], then ask for additional regexps to match buffers to kill.
If \\[universal-argument]\\[universal-argument], then turn off mu4e as well if necessary."
  (interactive "P")
  (cl-loop for timer in timer-idle-list
           if (eq (timer--function timer) 'pdf-cache--prefetch-start)
           do (cancel-timer timer))
  (let ((reg (when (equal arg '(4))
               (read-string "Additional regexp: "))))
    (cl-loop for buffer being the buffers
             do (and (is-not-needed-buffer buffer reg)
                     (kill-buffer (buffer-name buffer)))))
  (when (and (boundp 'recentf-list)
             (boundp 'durand-recently-closed-files))
    (setf recentf-list nil
          durand-recently-closed-files nil))
  ;; (cond
  ;;  ((and arg (or (get-process " *mu4e-proc*")
  ;;                mu4e~update-timer))
  ;;   (mu4e-quit)
  ;;   (setf mu4e~update-timer nil)
  ;;   (message "mu4e is turned off now.")))
  )

;;;###autoload
(defun durand-edit-special ()
  (interactive)
  (condition-case err
      (cond
       ((string-prefix-p "*Org Src" (buffer-name))
        (org-edit-src-exit))
       (t
        (org-edit-special current-prefix-arg)))
    ((user-error error)
     (pcase (cdr err)
       ((pred null)
        (message "%s" (car err)))
       (_
        (message "%s: %s" (car err) (cdr err)))))))

;;;###autoload
(defun transpose-chars-back-N (n)
  "Transpose chars back N if possible."
  (interactive "N")
  (if (< (- (point) (point-min)) n)
      (user-error "No character at %d before the point" n)
    (save-excursion
      (transpose-regions (- (point) n) (- (point) n -1)
			 (- (point) n -1) (- (point) n -2)))))

;;;###autoload
(defun transpose-chars-back-2 (&optional arg)
  "Transpose back 2 chars"
  (interactive "p")
  (let ((num (if (or (null arg)
                     (= arg 1))
                 2
               arg)))
    (transpose-chars-back-N num)))

;;;###autoload
(defun transpose-chars-back-3 (&optional arg)
  "Transpose back 3 chars"
  (interactive "p")
  (let ((num (if (or (null arg)
                     (= arg 1))
                 3
               arg)))
    (transpose-chars-back-N num)))

;;;###autoload
(defun transpose-chars-back-4 ()
  "Go back three characters and transpose charcaters"
  (interactive)
  (transpose-chars-back-N 4))

;;;###autoload
(defun durand-eval ()
  "Evaluate the expression either before point or directly following point"
  (interactive)
  (cond
   ((use-region-p)
    (eval-region (region-beginning) (region-end)))
   ((or (looking-at "\\s(")
        (and (looking-at "\\s\"")
             (null (nth 3 (syntax-ppss)))))
    (save-excursion
      (forward-sexp 1)
      (eval-last-sexp current-prefix-arg)))
   (t
    (eval-last-sexp current-prefix-arg))))
