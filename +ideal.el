;;; package --- Summary: Some custom functiionalities I think of as ideal to
;;; have.
;;
;; (define-prefix-command 'ideal-navigation)

;; (define-key ideal-mode-map [?a] ideal-navigation)

;; (define-key ideal-navigation [?a] (lambda () (interactive) (message "hi")))

;; The keys are not perfect yet. This is why I sometimes find it inconvenient to us. For
;; one thing, there are too more hydras. I think two hydras suffice, and it could be more
;; convenient as well.

;; add a convenient key to quit minibuffer
(require 'swiper)
(require 'dash)
(require 'cl-lib)
;; (require 'text-obj)
(define-key swiper-map [f1] 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map [f1] 'minibuffer-keyboard-quit)
(defvar durand-stop-timer nil "The timer to control stopping to take a rest!")
(defvar durand-back-timer nil "The timer to control going back to work!")



;; jumping stuff

;; buffer stuff

;;;###autoload
(defun durand-meta-buffer-p (name)
  "Determine whether the buffer is meta or not.
The buffer is considered meta if it is a live buffer and one of the following conditions is satisfied:
1. buffer name starts with *.
2. buffer name starts with a space.
3. it is in one of the \"meta\" modes: magit, dired, eww, mu4e, or elfeed."
  (and (buffer-live-p (get-buffer name))
       (or (= 42 (aref name 0)) ;; 42 => *
           (= 32 (aref name 0)) ;; 32 => space
           (with-current-buffer name
             (derived-mode-p 'magit-mode 'dired-mode 'eww-mode 'mu4e-view-mode 'mu4e-main-mode
                             'mu4e-headers-mode 'mu4e-compose-mode 'mu4e-org-mode
                             'mu4e~update-mail-mode 'elfeed-show-mode 'elfeed-search-mode)))))

;;;###autoload
(defun durand-next-real-buffer ()
  "Go to the next REAL buffer, as determined by the function `durand-meta-buffer-p'"
  (interactive)
  (let ((ori (current-buffer))
        (fois 0)
        found)
    (next-buffer)
    (while (and (null found)
                (< fois 10))
      (if (not (durand-meta-buffer-p (buffer-name)))
          (setf found t)
        (incf fois)
        (next-buffer)))
    (unless found
      (switch-to-buffer ori))))

;;;###autoload
(defun durand-previous-real-buffer ()
  "Go to the previous REAL buffer, as determined by the function `durand-meta-buffer-p'"
  (interactive)
  (let ((ori (current-buffer))
        (fois 0)
        found)
    (previous-buffer)
    (while (and (null found)
                (< fois 10))
      (if (not (durand-meta-buffer-p (buffer-name)))
          (setf found t)
        (incf fois)
        (previous-buffer)))
    (unless found
      (switch-to-buffer ori))))

;;;###autoload
(defun durand-next-meta-buffer ()
  "Go to the next REAL buffer, as determined by the function `durand-meta-buffer-p'"
  (interactive)
  (let ((ori (current-buffer))
        (limit 10)
        (fois 0)
        found)
    (next-buffer)
    (while (and (null found)
                (< fois limit))
      (if (durand-meta-buffer-p (buffer-name))
          (setf found t)
        (incf fois)
        (next-buffer)))
    (unless found
      (switch-to-buffer ori))))

;;;###autoload
(defun durand-previous-meta-buffer ()
  "Go to the next REAL buffer, as determined by the function `durand-meta-buffer-p'"
  (interactive)
  (let ((ori (current-buffer))
        (limit 10)
        (fois 0)
        found)
    (previous-buffer)
    (while (and (null found)
                (< fois limit))
      (if (durand-meta-buffer-p (buffer-name))
          (setf found t)
        (incf fois)
        (previous-buffer)))
    (unless found
      (switch-to-buffer ori))))



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

(map! :map doom-leader-toggle-map [?t] #'durand-show-current-time)

;; add a custom keymap
(define-prefix-command 'durand-switch-buffer-map)
(define-key durand-switch-buffer-map [?\C-c ?q] 'minibuffer-keyboard-quit)
(define-key durand-switch-buffer-map [?\C-c ?h] 'durand-toggle-headlong)
(define-key durand-switch-buffer-map [?\C-c ?s] (lambda ()
						  (interactive)
						  (message "%s" durand-headlong)))

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
        ("ideal.el" . (lambda ()
                        (ignore-errors
                          (goto-char (point-min))
                          (search-forward "defhydra" nil 'go 2)
                          (forward-line 0)
                          (recenter 0))))
        ("\\(diary\\|notes\\).org" . (lambda ()
                                       (goto-char (point-max))
                                       (org-show-context)
                                       (recenter -3)))))

;;;###autoload
(defun durand-tex-action ()
  "Performed when jumping to a tex file"
  (tex-first-pdf))

(defvar durand-jumped nil
  "Determine if `durand-bookmark-jump-headlong' executes the hooks at the end")

;;;###autoload
(defun durand-bookmark-jump-headlong ()
  "Inspired by bookmark-jump-headlong, but using ivy-read:
If there is only one match, then perform the default action"
  (interactive)
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
    (let* ((cur-name (buffer-name))
           (action (assoc-default cur-name durand-jump-hook-alist
                                  (lambda (element key)
                                    (string-match element key)))))
      (cond
       ((not (null action))
        (funcall action))
       (t nil)))))



;; recentf stuff

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



;; headlong

;;;###autoload
(defun durand-self-insert-complete-and-exit ()
  "Taken from headlong file.
Insert the character you type and try to complete.
If this results in one candidate, then immediately exit the minibuffer with the default action."
  (interactive)
  (when durand-headlong
    (let ((candidates-length (let ((collection (ivy-state-collection ivy-last)))
                               (cond
                                ((sequencep collection)
                                 (safe-length (ivy--filter ivy-text collection)))
                                ((functionp collection)
                                 (safe-length (funcall collection ivy-text)))))))
      (cond
       ((= 1 candidates-length)
	(if durand-changed-p
	    (ivy-done)
	  (setq durand-changed-p t)))
       (t (setq durand-changed-p t))))))

(defvar durand-headlong t
  "Whether to use headlong mode or not.
Default to t")

(defun reset-durand-headlong ()
  "Reset DURAND-HEADLONG"
  (setq durand-headlong t))

(defun durand-toggle-headlong ()
  "Toggle DURAND-HEADLONG"
  (interactive)
  (setq durand-headlong (not durand-headlong)))

(defvar durand-changed-p nil
  "A variable to determine if ivy-read is just initialised.")

(defun reset-durand-changed ()
  "Reset the variable DURAND-CHANGED-P to nil"
  (setq durand-changed-p nil))



;; update durand-recentf
(defun durand-update-recentf ()
  "Simple mapcar"
  (interactive)
  (setq durand-recentf-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list)))

;; kill from recentf
;; (defun durand-kill-from-recentf (x)
;;   "Kill the element X from recentf"
;;   (interactive)
;;   (setq recentf-list (delete (cdr (assoc x durand-recentf-list)) recentf-list)))



;; (defvar durand-recently-closed-files nil
;;   "A list of recently closed files")

;; (defvar durand-recently-closed-files-limit 10
;;   "Maximum number of files to store in `durand-recently-closed-files'")

;;;###autoload
;; (defun durand-kill-buffer ()
;;   "Kill buffer or bury-buffer"
;;   (interactive)
;;   (when (and (buffer-file-name)
;;              (file-exists-p (buffer-file-name)))
;;     (setf durand-recently-closed-files
;;           (reverse
;;            (remove-duplicates
;;             (reverse
;;              (push (buffer-file-name) durand-recently-closed-files))
;;             :test #'string=)))
;;     (when (> (length durand-recently-closed-files)
;;              durand-recently-closed-files-limit)
;;       (setcdr (nthcdr (1- durand-recently-closed-files-limit)
;;                       durand-recently-closed-files)
;;               nil)))
;;   (cond
;;    ((derived-mode-p 'magit-mode 'eww-mode 'mu4e-main-mode 'mu4e-org-mode
;;                     'mu4e~update-mail-mode 'elfeed-show-mode 'elfeed-search-mode)
;;     (if current-prefix-arg
;;         (kill-buffer (current-buffer))
;;       (bury-buffer)))
;;    ((derived-mode-p 'mu4e-headers-mode)
;;     (mu4e~headers-quit-buffer))
;;    ((derived-mode-p 'mu4e-compose-mode)
;;     (mu4e-message-kill-buffer))
;;    ((derived-mode-p 'mu4e-view-mode)
;;     (mu4e~view-quit-buffer))
;;    ((derived-mode-p 'org-agenda-mode)
;;     (org-agenda-exit))
;;    ((and (not (member* (buffer-name) '("*scratch*" "*Messages*")
;;                        :test #'string=))
;;          (buffer-modified-p) (y-or-n-p "Want to save?"))
;;     (save-buffer 0)
;;     (setf durand-recently-closed-files
;;           (reverse
;;            (remove-duplicates
;;             (reverse
;;              (push (buffer-file-name) durand-recently-closed-files))
;;             :test (lambda (x y) (string= x y)))))
;;     (when (> (length durand-recently-closed-files)
;;              durand-recently-closed-files-limit)
;;       (setcdr (nthcdr (1- durand-recently-closed-files-limit)
;;                       durand-recently-closed-files)
;;               nil))
;;     (kill-buffer (current-buffer)))
;;    ((and (string= (buffer-name) "*scratch*")
;;          (null (buffer-modified-p))
;;          buffer-read-only)
;;     (message "Je ne veux pas tuer ce tampon."))
;;    ((string= (buffer-name) "*scratch*")
;;     (kill-buffer (current-buffer))
;;     (switch-to-buffer "*scratch*")
;;     (read-only-mode)
;;     (fundamental-mode)
;;     (message "Le tampon est prêt maintenant."))
;;    (t
;;     (kill-buffer (current-buffer)))))



;; windows

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
(defun durand-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame.
Taken from `xah-next-window-or-frame'.
Version 2017-01-27"
  (interactive)
  (cond ((or current-prefix-arg (one-window-p))
         (other-frame 1))
        (t
         (other-window 1))))



;; buffer
;;;###autoload
(defun durand-switch-buffer (&optional buf)
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

(map! :map doom-leader-buffer-map "b" #'durand-switch-buffer)

;;;###autoload
;; (defvar durand-inactive-buffers nil
;;   "A list consisting of inactive buffers")

;;;###autoload
;; (defun durand-buffer-group (&optional buffer)
;;   "Return the group to which BUFFER belongs to.
;; If BUFFER is nil, then use the current buffer."
;;   (let ((buffer (or buffer (current-buffer))))
;;     (if (-contains? durand-inactive-buffers buffer)
;;         'inactive
;;       'active)))

;;;###autoload
;; (defun durand-mark-buffer-inactive ()
;;   "Mark the current buffer inactive"
;;   (interactive)
;;   (when (not (memq (current-buffer) durand-inactive-buffers))
;;     (push (current-buffer) durand-inactive-buffers)))

;;;###autoload
;; (defun durand-mark-buffer-active ()
;;   "Mark the current buffer active"
;;   (interactive)
;;   (setf durand-inactive-buffers (remove (current-buffer) durand-inactive-buffers)))

;;;###autoload
(defun durand-complete-buffer (str)
  "Complete buffers; intended to be used as `collection' of `ivy-read' with `dynamic-collection' set to t.
If STR starts with \"-\", then complete also with mode name;
if STR starts with \"/\", then complete also with the file name;
if STR starts with a space, then consider also hidden buffers."
  (cl-loop for buffer being buffers
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
           collect (buffer-name buffer)))

;;;###autoload
(defun durand-other-buffer ()
  "Switch to the other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun durand-new-buffer (&optional name)
  "Create a new buffer with name NAME; NAME defaults to *new*"
  (interactive)
  (let ((name (or name
                  (let ((n 1))
                    (while (get-buffer (format "sans titre<%d>" n))
                      (incf n))
                    (format "sans titre<%d>" n)))))
    (switch-to-buffer name)))



;; moving

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
  (let ((ori (point))
        (regex (if current-prefix-arg "\n\f" "\n[\t\n ]*\n+")))
    (cond
     ((and current-prefix-arg (equal current-prefix-arg '(4)))
      (re-search-backward regex nil 'go))
     ((equal (point) (save-excursion (beginning-of-visual-line) (point)))
      (if (not (re-search-backward regex nil 'go))
          (goto-char (point-min))
        (goto-char (match-beginning 0))
        (forward-char 1)))
     (t
      (beginning-of-visual-line)))))

;;;###autoload
(defun durand-end-of-line-or-block (&optional arg)
  "From `xah-end-of-line-or-block'.
Move cursor to end of line or next paragraph.
When called the first time, move to end of line
When called more than once, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04"
  (interactive "P")
  (let (($p (point))
        (regex (if current-prefix-arg "\n\f" "\n[\t\n ]*\n+")))
    (cond
     ((and current-prefix-arg (equal current-prefix-arg '(4)))
      (re-search-forward regex nil 'go))
     ((memq (point) (save-excursion (end-of-visual-line) (list (1- (point)) (point))))
      (re-search-forward regex nil 'go)
      (backward-char 1))
     (t
      (end-of-visual-line)))))

;;;###autoload
;; (defun durand-general-move (&optional arg)
;;   "The general function to move the cursor"
;;   (interactive "p")
;;   (pcase arg
;;     ((pred null)
;;      (durand-general-move 1))
;;     ((pred integerp)
;;      (pcase major-mode
;;        ('emacs-lisp-mode
;;         (if (or (region-active-p)
;;                 (lispy-left-p)
;;                 (lispy-right-p))
;;             (if (> arg 0)
;;                 (lispy-down arg)
;;               (lispy-up (* -1 arg)))
;;           (next-line arg)))
;;        ((guard (derived-mode-p 'org-mode))
;;         (if (org-at-heading-p)
;;             (ignore-errors
;;               (org-speed-move-safe
;;                (lambda () (interactive) (outline-next-visible-heading arg))))
;;           (next-line arg)))
;;        ((guard (derived-mode-p 'org-agenda-mode))
;;         (if (> arg 0)
;;             (let ((this-command 'org-agenda-next-item))
;;               (org-agenda-next-item arg))
;;           (let ((this-command 'org-agenda-previous-item))
;;             (org-agenda-previous-item (* -1 arg)))))
;;        (_
;;         (next-line arg))))
;;     (_
;;      (message "Weird arg: %s" arg))))

;;;###autoload
(defun durand-general-MOVE (&optional arg)
  "A wrapper around `durand-end-of-line-or-block' and `durand-beginning-of-line-or-block'."
  (interactive)
  (cond
   ((< arg 0)
    (durand-beginning-of-line-or-block current-prefix-arg))
   (t
    (durand-end-of-line-or-block current-prefix-arg))))

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
    ((guard (derived-mode-p 'tex-mode))
     (save-buffer 0)
     (setf durand-tex-action (lambda ()
                               "tex or display or kill"
                               (interactive)
                               (if current-prefix-arg
                                   (tex-display-or-kill-temp-buffer)
                                 (tex)))))
    (_
     (save-buffer 0))))

(defvar durand-tex-action nil
  "Do what when executing f9 in tex buffers")

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



;; search french words
;;;###autoload
(defun chercher-français (query)
  "Rechercher un mot dans la liste des mots français dans le fichier wiki.org"
  (interactive (list (read-string "Question: ")))
  (let* ((route_du_fichier "~/org/français/français.org")
         (nom_du_fichier "français.org")
         (a_tuer (not (get-buffer nom_du_fichier)))
         (chose (mapconcat #'identity
                           (org-ql--query route_du_fichier `(and (regexp ,query) (tags "mots"))
                             :action (lambda ()
                                       (let ((element (cadr (org-element-headline-parser (line-end-position)))))
                                         (concat
                                          (plist-get element :raw-value)
                                          ": "
                                          (plist-get element :MEANING)))))
                           "\n")))
    (if (and a_tuer (get-buffer nom_du_fichier))
        (kill-buffer nom_du_fichier))
    (if (/= (length chose) 0)
        (with-current-buffer-window "*mots*" nil nil
                                    (insert chose))
      (define-word query 'wordreference))))



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

(defface durand-pdf-dir-face '((t :foreground "orange2"))
  "Face for directory in durand-pdf-mode")

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

(map! :map durand-pdf-mode-map
      [?q] 'quit-window
      [?N] 'forward-line
      [?n] 'durand-pdf-next-pdf-line
      [?P] (lambda () (interactive) (forward-line -1))
      [?p] 'durand-pdf-previous-pdf-line
      [return] 'durand-pdf-open-pdf
      [32] 'durand-pdf-open-or-scroll-up
      [backspace] 'durand-pdf-open-or-scroll-down
      [?o] 'kill-other-buffer-window
      [?k] 'kill-current-buffer)

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

;; ;; pop up rule for pdf buffer
;; ;; I prefer full-screen view of search results.
;; (set-popup-rule! "\\*chercher pdf\\*"
;;   :quit t
;;   :modeline nil
;;   :size 0.5
;;   :ttl 5
;;   :select t)

(set-evil-initial-state!
  '(durand-pdf-mode)
  'emacs)



;; editing - copying and cutting

;;;###autoload
;; (defun durand-copy-line-or-region ()
;;   "Copy current line, or region.
;; When called repeatedly, append copy subsequent lines.
;; Adapted from `xah-copy-line-or-region'."
;;   (interactive)
;;   (if (use-region-p)
;;       (copy-region-as-kill 0 0 t)
;;     (if (eq last-command this-command)
;;         (when (not (eobp))
;;           (kill-append "\n" nil)
;;           (kill-append
;;            (buffer-substring-no-properties (line-beginning-position) (line-end-position))
;;            nil))
;;       (if (eobp)
;;           (when (not (eq (char-before) 10))
;;             (copy-region-as-kill (line-beginning-position) (line-end-position)))
;;         (copy-region-as-kill (line-beginning-position) (line-end-position))))))



;; editing - yanking

;;;###autoload
;; (defun durand-general-yank ()
;;   "General yank function"
;;   (interactive)
;;   (forward-char 1)
;;   (if (eq last-command 'yank)
;;       (counsel-yank-pop)
;;     (pcase major-mode
;;       ((guard (memq 'lispy-mode minor-mode-list))
;;        (lispy-yank))
;;       ((guard (derived-mode-p 'org-mode))
;;        (org-yank))
;;       (_
;;        (counsel-yank-pop)))))



;; editing - killing

;;;###autoload
;; (defun durand-general-kill (&optional direction unit no-kill-p no-inner-p)
;;   "General function for killing.
;; In mu4e-headers-mode, execute `mu4e-headers-mark-for-delete'.
;; In org-agenda-mode, execute `org-agenda-kill'.
;; In dired-mode,execute `dired-do-delete'.
;; If the region is active, then delete or kill the region according to NO-KILL-P.
;; Otherwise, it behaves according to UNIT.
;; If UNIT is character, then it deletes or kills the character that follows or precedes the point.
;; If UNIT is word, then do the same for words.
;; If UNIT is line, then do the same for lines.
;; If UNIT is end, DIRECTION is forward, and if lispy-mode is on, then execute `lispy-kill';
;; else deletes or kills to the end or the beginning of the visual line.
;; If UNIT is char-or-bracket, then delete or kill the entire bracket near point, if the cursor is near a bracket;
;; with NO-INNER-P, only deletes or kills the brackets;
;; if point not near a bracket, then delete or kill a character."
;;   (interactive)
;;   (pcase major-mode
;;     ('mu4e-headers-mode
;;      (mu4e-headers-mark-for-delete))
;;     ((guard (derived-mode-p 'org-agenda-mode))
;;      (org-agenda-kill))
;;     ((guard (derived-mode-p 'dired-mode))
;;      (dired-do-delete current-prefix-arg))
;;     ((guard (use-region-p))
;;      (if no-kill-p
;;          (delete-region (region-beginning) (region-end))
;;        (kill-region (region-beginning) (region-end) t)))
;;     (_
;;      ;; this should be replaced by using text-objects.
;;      (let* ((direction (or direction 'forward))
;;             (unit (or unit 'character))
;;             (direction-factor (pcase direction
;;                                 ('forward 1)
;;                                 ('backward -1)
;;                                 (_ (user-error "Unknown direction: %s" direction))))
;;             (kill-function (if no-kill-p 'delete-region 'kill-region))
;;             (fois (prefix-numeric-value current-prefix-arg))
;;             (current-prefix-arg nil))
;;        (while (> fois 0)
;;          (decf fois)
;;          (pcase unit
;;            ('character
;;             (let* ((p1 (point))
;;                    (p2 (+ (point) direction-factor))
;;                    (beg (min p1 p2))
;;                    (end (max p1 p2)))
;;               (funcall kill-function beg end)))
;;            ('word
;;             (pcase direction
;;               ('forward
;;                (funcall kill-function (point) (progn (forward-word 1) (point))))
;;               ('backward
;;                (funcall kill-function (point) (progn (forward-word -1) (point))))))
;;            ('line
;;             (funcall kill-function (line-beginning-position) (line-beginning-position 2)))
;;            ('end
;;             (pcase direction
;;               ('forward
;;                (if (memq 'lispy-mode minor-mode-list)
;;                    (lispy-kill)
;;                  (funcall kill-function (point) (max (1+ (point)) (save-excursion
;;                                                                     (end-of-visual-line)
;;                                                                     (point))))))
;;               ('backward
;;                (funcall kill-function
;;                         (min (1- (point)) (save-excursion (beginning-of-visual-line) (point)))
;;                         (point)))))
;;            ('char-or-bracket
;;             (pcase direction-factor
;;               (1 (cond
;;                   ((or
;;                     (looking-at "\\s(")
;;                     (and (looking-at "\\s\"") (not (nth 3 (syntax-ppss)))))
;;                    (condition-case nil
;;                        (if (null no-inner-p)
;;                            (funcall kill-function (point) (save-excursion (forward-sexp 1) (point)))
;;                          (save-excursion
;;                            (forward-sexp 1)
;;                            (funcall kill-function (1- (point)) (point)))
;;                          (funcall kill-function (point) (1+ (point))))
;;                      (scan-error
;;                       (durand-general-kill 'forward 'character no-kill-p no-inner-p))))
;;                   ((looking-at "\\s)")
;;                    (condition-case nil
;;                        (if (null no-inner-p)
;;                            (progn
;;                              (forward-char 1)
;;                              (funcall kill-function
;;                                       (save-excursion (backward-sexp 1)
;;                                                       (point))
;;                                       (point)))
;;                          (save-excursion
;;                            (forward-char 1)
;;                            (backward-sexp 1)
;;                            (funcall kill-function (point) (1+ (point))))
;;                          (funcall kill-function (point) (1+ (point))))
;;                      (scan-error
;;                       (durand-general-kill 'forward 'character no-kill-p no-inner-p))))
;;                   (t
;;                    (durand-general-kill 'forward 'character no-kill-p no-inner-p))))
;;               (-1 (cond
;;                    ((or
;;                      (looking-back "\\s)" 1)
;;                      (and (looking-back "\\s\"" 1) (not (nth 3 (syntax-ppss)))))
;;                     (condition-case nil
;;                         (if (null no-inner-p)
;;                             (funcall kill-function (save-excursion (forward-sexp -1) (point)) (point))
;;                           (save-excursion
;;                             (forward-sexp -1)
;;                             (funcall kill-function (point) (1+ (point))))
;;                           (funcall kill-function (1- (point)) (point)))
;;                       (scan-error
;;                        (durand-general-kill 'backward 'character no-kill-p))))
;;                    ((looking-back "\\s(" 1)
;;                     (condition-case nil
;;                         (if (null no-inner-p)
;;                             (progn
;;                               (forward-char -1)
;;                               (funcall kill-function
;;                                        (point)
;;                                        (save-excursion (forward-sexp 1) (point))))
;;                           (save-excursion
;;                             (forward-char -1)
;;                             (forward-sexp 1)
;;                             (funcall kill-function (1- (point)) (point)))
;;                           (funcall kill-function (1- (point)) (point)))
;;                       (scan-error
;;                        (durand-general-kill 'backward 'character no-kill-p))))
;;                    (t
;;                     (durand-general-kill 'backward 'character no-kill-p))))))
;;            (_
;;             (user-error "Killing for %s not implemented yet" unit))))))))



;; editing - change pair

;;;###autoload
(defun durand-change-pair-beg ()
  "Change brackts. Assume point is at beginning of bracket"
  (interactive)
  (unless (or (looking-at "\\s(")
              (and (looking-at "\\s\"")
                   (not (nth 3 (syntax-ppss)))))
    (user-error "Not in front of a bracket"))
  (let* ((current-prefix-arg '(4))
         (to-pair (ivy-read "Choose one pair to change to: "
                            '("()" "[]" "<>"
                              "{}" "\"\"" "''"
                              "**" "--" "//" "__"
                              "++" "==" "||" "$$" "\\[\\]"
                              "\\(\\)" "\\{\\}")))
         (to-beg (substring-no-properties to-pair 0 (/ (length to-pair) 2)))
         (to-end (substring-no-properties to-pair (/ (length to-pair) 2) nil))
         (rend (prog2
                   (mark-sexp)
                   (region-end)
                 (deactivate-mark))))
    (save-excursion (goto-char rend)
                    (backward-delete-char 1)
                    (insert to-end))
    (insert to-beg)
    (delete-char 1)
    (backward-char 1)))

;;;###autoload
(defun durand-change-pair-end ()
  "Change brackts. Assume point is at end of bracket"
  (interactive)
  (unless (or (looking-back "\\s)" 1)
              (and (looking-back "\\s\"" 1)
                   (not (nth 3 (syntax-ppss)))))
    (user-error "Not after a bracket"))
  (save-excursion
    (forward-sexp -1)
    (durand-change-pair-beg))
  (forward-char 1))

;;;###autoload
(defun durand-change-pair ()
  "Change pair either in the front or at the end"
  (interactive)
  (cond
   ((or (looking-back "\\s)" 1)
        (and (looking-back "\\s\"" 1)
             (not (nth 3 (syntax-ppss)))))
    (durand-change-pair-end))
   ((or (looking-at "\\s(")
        (and (looking-at "\\s\"")
             (not (nth 3 (syntax-ppss)))))
    (durand-change-pair-beg))
   (t
    (user-error "Not near brackets"))))



;; editing - toggle cases

(defvar durand-case-state nil
  "State for cycling cases")

;;;###autoload
(defun durand-cycle-cases (&optional word)
  "Cycle cases for the word under point, or WORD if non-nil"
  (interactive)
  (when (not (eq last-command this-command))
    (setf durand-case-state nil))
  (if word
      (durand-cycle-cases-internal word)
    (if (not (thing-at-point 'word))
        (user-error "Not at a word")
      (let ((bounds (bounds-of-thing-at-point 'word))
            (new (durand-cycle-cases-internal (thing-at-point 'word)))
            (ori (point)))
        (delete-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (insert new)
        (goto-char ori)))))

;;;###autoload
(defun durand-cycle-cases-internal (word)
  "Cycle cases for WORD"
  (when current-prefix-arg
    (ivy-read "Choose what to do: " '("Capitalize" "Upper case" "Lower case")
              :action '(1
                        ("o" (lambda (x)
                               (pcase x
                                 ("Capitalize"
                                  (setf durand-case-state nil))
                                 ("Upper case"
                                  (setf durand-case-state 'caped))
                                 ("Lower case"
                                  (setf durand-case-state 'uped))))))))
  (pcase durand-case-state
    ((pred null)
     (setf durand-case-state 'caped)
     (capitalize word))
    ('caped
     (setf durand-case-state 'uped)
     (upcase word))
    ('uped
     (setf durand-case-state nil)
     (downcase word))
    (_
     (user-error "durand-case-state is strange: %s" durand-case-state))))

;;;###autoload
(defun durand-cap-word ()
  "Capitalize the previous word."
  (interactive)
  (let ((durand-case-state nil)
        (cur-pos (point)))
    (unless (and (looking-back "\\Sw" 1)
                 (looking-at "\\sw"))
      (forward-word -1))
    (durand-cycle-cases)
    (goto-char cur-pos)))

;;;###autoload
(defun upcase-region-or-word ()
  "upcase region or word"
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word 1)))

;;;###autoload
(defun downcase-region-or-word ()
  "downcase region or word"
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word 1)))

;;;###autoload
(defun capitalize-region-or-word ()
  "capitalize region or word"
  (interactive)
  (if (use-region-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word 1)))



;; editing - comment

;;;###autoload
;; (defun durand-comment-dwim (&optional arg) ;
;;   "Like `comment-dwim', but with current-prefix-arg, call `comment-line'."
;;   (interactive "P")
;;   (durand-operate-on-text-object ?: 'tobj-mark-region nil t)
;;   (comment-dwim arg))

(require 'evil)

(evil-define-operator evil-commenter (beg end type &optional arg)
  "`comment-dwim' on selected regions.
ARG is passed to `comment-dwim'."
  (interactive "<R>P")
  (pcase type
    ('block
        (apply-on-rectangle (lambda (x y)
                              (save-mark-and-excursion
                                (goto-char y)
                                (push-mark nil t t)
                                (goto-char x)
                                (exchange-point-and-mark)
                                (comment-dwim arg)))
                            beg end))
    (_
     (save-mark-and-excursion
       (goto-char end)
       (push-mark nil t t)
       (goto-char beg)
       (exchange-point-and-mark)
       (comment-dwim arg)))))



;; editing - align

(setf align-default-spacing 1)

(evil-define-operator evil-align-regexp (beg end type &optional arg)
  "Align regions by `align-regexp'.
Treat block selections as selecting lines.
And ARG behaves like in `align-regexp'."
  :move-point nil
  (interactive "<R>P")
  (let ((beg (save-excursion
               (goto-char beg)
               (line-beginning-position)))
        (end (save-excursion
               (goto-char end)
               (line-end-position)))
        (arguments (if arg
                       (list (read-string "Complex align using regexp: "
                                          "\\(\\s-*\\)" 'align-regexp-history)
                             (string-to-number
                              (read-string
                               "Parenthesis group to modify (justify if negative): " "1"))
                             (string-to-number
                              (read-string "Amount of spacing (or column if negative): "
                                           (number-to-string align-default-spacing)))
                             (y-or-n-p "Repeat throughout line? "))
                     (list (concat "\\(\\s-*\\)"
                                   (read-string "Align regexp: "))
                           1 align-default-spacing nil))))
    (save-excursion
      (align-regexp beg end
                    (nth 0 arguments)
                    (nth 1 arguments)
                    (nth 2 arguments)
                    (nth 3 arguments)))))




;; editing - wrap region

(defvar durand-custom-pairs '("()" "[]" "<>"
                              "{}" "\"\"" "''"
                              "**" "--" "//" "__"
                              "++" "==" "||" "$$" "\\[\\]"
                              "\\(\\)" "\\{\\}")
  "Some custom pairs to wrap regions.")

;;;###autoload
(defun durand-wrap-region-with ()
  "Wrap region with a chosen delimiter"
  (interactive)
  (let* ((pair (ivy-read "Choose a pair: " durand-custom-pairs))
         (left (substring-no-properties pair 0 (/ (length pair) 2)))
         (right (substring-no-properties pair (/ (length pair) 2) nil)))
    (wrap-region-with left right)))



;; editing - open line

;;;###autoload
(defun durand-open-line (&optional direction n)
  "Open N lines in the direction DIRECTION"
  (interactive)
  (let ((n (or n (prefix-numeric-value current-prefix-arg)))
        (direction (or direction 'down)))
    (unless (and (integerp n)
                 (> n 0))
      (user-error "Invalid number: %s" n))
    (pcase direction
      ('up
       (let ((pos (save-excursion
                    (search-backward "\n" nil 'go)
                    (point))))
         (setf (buffer-substring pos pos)
               (make-string n ?\n))
         (forward-line -1)
         (indent-according-to-mode)))
      ('down
       (let ((pos (save-excursion
                    (search-forward "\n" nil 'go)
                    (point))))
         (setf (buffer-substring pos pos)
               (make-string n ?\n))
         (forward-line n)
         (indent-according-to-mode)))
      (_
       (user-error "Invalid direction %s" direction)))))



;; marking - line

;;;###autoload
(defun durand-mark-line ()
  "Mark the current line or extending the region downwards"
  (interactive)
  (if (use-region-p)
      (if (/= (region-beginning) (point))
          ;; point at the end
          (progn
            (forward-line 1)
            (end-of-line 1))
        (exchange-point-and-mark)
        (forward-line 1)
        (end-of-line 1))
    (push-mark (line-beginning-position) nil t)
    (end-of-line)))



;; marking - block

;;;###autoload
(defun durand-mark-block ()
  "Mark the current block or extending the region downwards"
  (interactive)
  (if (use-region-p)
      (if (/= (region-beginning) (point))
          ;; point at the end
          (progn
            (skip-chars-forward "\n\t ")
            (when (re-search-forward "\n[\n\\s-]*\n" nil 'go)
              (goto-char (match-beginning 0))))
        (exchange-point-and-mark)
        (progn
          (skip-chars-forward "\n\t ")
          (when (re-search-forward "\n[\n\\s-]*\n" nil 'go)
            (goto-char (match-beginning 0)))))
    (when (re-search-backward "\n[\n\\s-]*\n" nil 'go)
      (goto-char (match-end 0)))
    (push-mark nil nil t)
    (when (re-search-forward "\n[\n\\s-]*\n" nil 'go)
      (goto-char (match-beginning 0)))))

;; (global-set-key (kbd "<home>") 'general-hydra/body)

(global-set-key [f6] nil)
(global-set-key [f11] nil)



;; evaling

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



;; documenting

;;;###autoload
;; (defun durand-search-general-hydra ()
;;   "Seaerch the keys in `general-hydra'"
;;   (interactive)
;;   (let* ((max-length (apply #'max (mapcar (lambda (x)
;;                                             (length (car x)))
;;                                           general-hydra/heads)))
;;          (heads-alist (mapcar (lambda (x)
;;                                 (cons
;;                                  (format "%s%s%s"
;;                                          (car x)
;;                                          (make-string (- (+ 2 max-length) (length (car x))) 32)
;;                                          (cadr x))
;;                                  x))
;;                               general-hydra/heads))
;;          (clé (cdr (assoc (ivy-read "Chois un clé: " heads-alist) heads-alist))))
;;     (with-help-window (help-buffer)
;;       (princ (format "%s\n\n" (cadr clé)))
;;       (condition-case nil
;;           (princ (format "%s" (documentation (cadr clé))))
;;         (error nil)))))



;; editing - replacing

(defun durand-replace-char-or-region (&optional arg)
  "Replace the text in region or the char following cursor;
with ARG (4), replace the char preceding the cursor;
with ARG (16) or if at the end of the visual line, append to the end of the visual line;
with ARG 0 replace the following with a string instead of a character."
  (interactive "P")
  (cond
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (region-end))
          (text (read-string "Replace with: ")))
      (setf (buffer-substring beg end) text)
      (forward-char (length text))))
   ((equal arg '(4))
    (setf (buffer-substring (1- (point)) (point)) (read-char "Replace with: "))
    (forward-char 1))
   ((= (point) (save-excursion (end-of-visual-line) (point)))
    (insert (read-string "Append: "))
    (end-of-visual-line))
   ((equal arg '(16))
    (save-excursion
      (end-of-visual-line)
      (insert (read-string "Append: "))))
   ((equal arg 0)
    (setf (buffer-substring (point) (1+ (point))) (read-string "Replace with: ")))
   (t
    (setf (buffer-substring (point) (1+ (point))) (read-char "Replace with: ")))))



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



;; browsing
(defun durand-open-browser (&optional arg)
  "Open Safari.
With ARG \\[universal-argument], prompt for some frequently visited websites.
With ARG \\[universal-argument]\\[universal-argument], close safari."
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



;; searching

;;;###autoload
;; (defun durand-search-last-term ()
;;   "Search the last term in swiper"
;;   (interactive)
;;   (swiper (car swiper-history)))





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

(defun is-not-needed-buffer (buf)
  "Match some buffers I do not want to keep around"
  (let ((name (buffer-name buf)))
    (and
     (or (and (= ?* (aref name 0))
              (not (string-match "^\\*scratch\\*$" name)))
         (string-match "^magit" name)
         (string-match "^TAGS\\(<.*>\\)?$" name))
     (null (get-buffer-process name)))))


(defun clean-up-buffers (&optional arg)
  "Clean up some buffers that I oft do not need to keep around and kill unnecessary timers;
If the buffer has a running process, then do not kill it.
If ARG is non-nil, then turn off mu4e as well if necessary."
  (interactive "P")
  (cl-loop for timer in timer-idle-list
           if (eq (timer--function timer) 'pdf-cache--prefetch-start)
           do (cancel-timer timer))
  (cl-loop for buffer being the buffers
           do (and (is-not-needed-buffer buffer)
                   (kill-buffer (buffer-name buffer))))
  (when (and (boundp 'recentf-list)
             (boundp 'durand-recently-closed-files))
    (setf recentf-list nil
          durand-recently-closed-files nil))
  (cond
   ((and arg (or (get-process " *mu4e-proc*")
                 mu4e~update-timer))
    (mu4e-quit)
    (setf mu4e~update-timer nil)
    (message "mu4e is turned off now."))))


;; org-edit-special

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
;; (defhydra general-hydra (:hint nil :color amaranth
;;                                :pre (progn
;;                                       ;; (set-face-attribute 'durand-custom-mode-face nil :foreground "#5ea3bd")
;;                                       (set-face-attribute 'durand-custom-mode-face nil :foreground "cornflowerblue")
;;                                       (set-frame-parameter nil 'cursor-type 'box)
;;                                       (setf durand-custom-modeline (propertize "G"
;;                                                                                'help-echo "général")))
;;                                :post (progn
;;                                        (set-face-attribute 'durand-custom-mode-face nil :foreground "goldenrod1")
;;                                        (set-frame-parameter nil 'cursor-type 'bar)
;;                                        (setq durand-custom-modeline (propertize "S"
;;                                                                                 'help-echo "spécifique"))))
;;   "
;; general hydra"
;;   ("k" previous-line)
;;   ("j" next-line)
;;   ("h" backward-char)
;;   ("l" forward-char)
;;   ;; ("o" durand-beginning-of-line-or-block)
;;   ("p" durand-general-yank)
;;   ("<backspace>" (switch-to-buffer (other-buffer)))
;;   ("SPC b" durand-switch-buffer)
;;   ;; ("u" backward-word)
;;   ("P" forward-sentence)
;;   ("SPC P" backward-sentence)
;;   ;; ("i" forward-word)
;;   ("i" (durand-move-text-object ?u))
;;   ("w" (durand-move-text-object 0 ?w (if (null current-prefix-arg)
;;                                          0
;;                                        (prefix-numeric-value current-prefix-arg))))
;;   ("W" (durand-move-text-object 0 ?W (if (null current-prefix-arg)
;;                                          0
;;                                        (prefix-numeric-value current-prefix-arg))))
;;   ("b" (durand-move-text-object 0 ?b (if (null current-prefix-arg)
;;                                          0
;;                                        (prefix-numeric-value current-prefix-arg))))
;;   ("B" (durand-move-text-object 0 ?B (if (null current-prefix-arg)
;;                                          0
;;                                        (prefix-numeric-value current-prefix-arg))))
;;   ("e" (durand-move-text-object 0 ?e (if (null current-prefix-arg)
;;                                          0
;;                                        (prefix-numeric-value current-prefix-arg))))
;;   ("E" (durand-move-text-object 0 ?E (if (null current-prefix-arg)
;;                                          0
;;                                        (prefix-numeric-value current-prefix-arg))))
;;   ("à" (durand-general-MOVE -1))
;;   (")" (durand-general-MOVE 1))
;;   ("u" undo-tree-undo)
;;   ("U" undo-tree-redo)
;;   ("RET u" undo-tree-visualize)
;;   ("t" transpose-chars)
;;   ("T" durand-show-current-time)
;;   ("&" transpose-chars-back-2)
;;   ("é" transpose-chars-back-3)
;;   ("\"" transpose-chars-back-4)
;;   ("'" fill-paragraph)
;;   ("SPC '" (condition-case err
;;                (cond
;;                 ((string-prefix-p "*Org Src" (buffer-name))
;;                  (org-edit-src-exit))
;;                 (t
;;                  (org-edit-special current-prefix-arg)))
;;              ((user-error error)
;;               (pcase (cdr err)
;;                 ((pred null)
;;                  (message "%s" (car err)))
;;                 (_
;;                  (message "%s: %s" (car err) (cdr err)))))))
;;   ("ù" (if (string= major-mode "org-mode")
;;            (org-advance current-prefix-arg)
;;          (user-error "Not in org mode buffer")))
;;   ("$ù" (if (string= major-mode "org-mode")
;;             (org-retreat current-prefix-arg)
;;           (user-error "Not in org mode buffer")))
;;   ("%" widen)
;;   ("$ DEL" back-to-indentation)
;;   ("$$" (recenter 0))
;;   ("$m" (recenter (/ (window-body-height) 2)))
;;   ("$-" (swiper (thing-at-point 'word)))
;;   ("*" (recenter -3))
;;   ("=" counsel-grep-or-swiper)
;;   ("+" eval-expression)
;;   ("," (durand-buffer-scroll 'up) :color blue)
;;   (";" (durand-buffer-scroll 'down) :color blue)
;;   ("?" (durand-buffer-scroll 'up 1) :color blue)
;;   ("." (durand-buffer-scroll 'down 1) :color blue)
;;   ("§" (durand-buffer-scroll 'up nil t) :color blue)
;;   ("è" (durand-buffer-scroll 'down nil t) :color blue)
;;   ("!" (durand-buffer-scroll 'up 1 t) :color blue)
;;   ("ç" (durand-buffer-scroll 'down 1 t) :color blue)
;;   ("RET w" durand-next-window-or-frame)
;;   ("RET W" ace-select-window)
;;   ("q" quit-window)
;;   ("Q" quit-other-window)
;;   ("<f10>" durand-general-save-buffer)
;;   ("<f9>" (when (functionp durand-tex-action) (funcall durand-tex-action)))
;;   ("s-m" durand-toggle-mode-line)
;;   ("y" (durand-operate-on-text-object ?y 'copy-region-as-kill nil t))
;;   ;; ("<" (durand-general-kill 'forward 'character))
;;   ;; (">" (durand-general-kill 'backward 'character))
;;   ;; ("f" (progn
;;   ;;        (iy-go-to-char (prefix-numeric-value current-prefix-arg) (read-char "Jump to: "))
;;   ;;        (iy-hydra/body)) :color blue)
;;   ;; ("F" (progn
;;   ;;        (iy-go-to-char-backward (prefix-numeric-value current-prefix-arg) (read-char "Jump to: "))
;;   ;;        (iy-hydra/body)) :color blue)
;;   ("d" (durand-operate-on-text-object ?d 'kill-region))
;;   ("D" (durand-operate-on-text-object ?D 'delete-region))
;;   ("c" (durand-operate-on-text-object ?c 'kill-region t) :color blue)
;;   ("C" (durand-operate-on-text-object ?C 'kill-region nil nil ?$) :color blue)
;;   ;; ("c" (durand-operate-on-text-object ?c 'copy-region-as-kill nil t))
;;   ;; ("dP" (kill-sentence (prefix-numeric-value current-prefix-arg)))
;;   ;; ("dO" (kill-sentence (- (prefix-numeric-value current-prefix-arg))))
;;   ;; ("di" (durand-general-kill 'forward 'word))
;;   ;; ("dI" (durand-general-kill 'forward 'word t))
;;   ;; ("du" (durand-general-kill 'backward 'word))
;;   ;; ("dU" (durand-general-kill 'backward 'word t))
;;   ;; ("dp" (durand-general-kill 'forward 'end))
;;   ;; ("do" (durand-general-kill 'backward 'end))
;;   ;; ("dd" (durand-general-kill nil 'line))
;;   ;; ("dx" (durand-general-kill nil 'line t))
;;   ;; ("dc" durand-copy-line-or-region)
;;   ;; ("dh" (durand-general-kill 'backward 'char-or-bracket))
;;   ;; ("dj" (progn
;;   ;;         (beginning-of-visual-line)
;;   ;;         (set-mark-command nil)
;;   ;;         (next-line 2)
;;   ;;         (durand-general-kill)))
;;   ;; ("dk" (progn
;;   ;;         (end-of-visual-line)
;;   ;;         (set-mark-command nil)
;;   ;;         (previous-line)
;;   ;;         (beginning-of-visual-line)
;;   ;;         (forward-char -1)
;;   ;;         (durand-general-kill)))
;;   ;; ("dl" (durand-general-kill 'forward 'char-or-bracket))
;;   ;; ("dH" (durand-general-kill 'backward 'char-or-bracket nil t))
;;   ;; ("dL" (durand-general-kill 'forward 'char-or-bracket nil t))
;;   ;; ("dep" (progn
;;   ;;          (mark-paragraph)
;;   ;;          (durand-general-kill)))
;;   ;; ("dap" (progn
;;   ;;          (mark-paragraph)
;;   ;;          (durand-general-kill)))
;;   ;; ("deb" (progn
;;   ;;          (durand-mark-block)
;;   ;;          (durand-general-kill)))
;;   ;; ("dab" (progn
;;   ;;          (durand-mark-block)
;;   ;;          (durand-general-kill)))
;;   ;; ("de\"" (progn
;;   ;;           (er/mark-inside-pairs)
;;   ;;           (durand-general-kill)))
;;   ;; ("da\"" (progn
;;   ;;           (er/mark-outside-pairs)
;;   ;;           (durand-general-kill)))
;;   ("m" set-mark-command)
;;   ("v" (durand-operate-on-text-object ?v 'tobj-mark-region nil t))
;;   ("g" durand-kill-buffer)
;;   ("G" durand-change-pair)
;;   ("s-g" (message "Général"))
;;   ("s" durand-general-save-buffer)
;;   ("S" cycle-spacing)
;;   ("z" downcase-region-or-word)
;;   ("Z" upcase-region-or-word)
;;   ("SPC z" capitalize-region-or-word)
;;   ("C-SPC" set-mark-command)
;;   (":" durand-comment-dwim)
;;   ;; ("/" swiper-all)
;;   ("r" (er/expand-region 1))
;;   ("R" (er/contract-region 1))
;;   ("SPC e" durand-wrap-region-with)
;;   ;; ("r" durand-replace-char-or-region)
;;   ("x" amx)
;;   ("X" (pcase major-mode
;;          ('mu4e-headers-mode
;;           (mu4e-mark-execute-all t))
;;          (_
;;           (user-error "Not in mu4e-headers-mode"))))
;;   ("o" durand-open-line :color blue)
;;   ("O" (durand-open-line 'up) :color blue)
;;   ("n" durand-search-last-term)
;;   ("<S-backspace>" durand-next-real-buffer)
;;   ("SPC <S-backspace>" durand-previous-real-buffer)
;;   ("-" durand-next-meta-buffer)
;;   ("_" durand-previous-meta-buffer)
;;   ("<home>" nil :color blue)
;;   ("<tab>" (cond
;;             ((derived-mode-p 'org-mode)
;;              (org-cycle current-prefix-arg))
;;             (t
;;              (indent-for-tab-command current-prefix-arg))))
;;   ("(" hydra--universal-argument)
;;   ("a" (goto-char
;;         (min (1+ (point))
;;              (save-excursion (end-of-visual-line) (point))))
;;    :color blue)
;;   ("RET a" (org-agenda nil "o") :color blue)
;;   ("A" end-of-visual-line :color blue)
;;   ("SPC RET r" string-rectangle)
;;   ("SPC RET d" delete-rectangle)
;;   ("SPC RET b" org-open-bookmarks)
;;   ("SPC RET n" org-open-novels)
;;   ;; ("SPC RET N" org-update-novels)
;;   ("SPC RET l" org-open-articles)
;;   ("SPC RET g" revert-buffer)
;;   ("RET x" exchange-point-and-mark)
;;   ("RET SPC" (deactivate-mark))
;;   ("RET ," (move-to-window-line 0))
;;   ("RET ;" move-to-window-line)
;;   ("RET :" (move-to-window-line -2))
;;   ("RET p" mark-paragraph)
;;   ("RET l" durand-mark-line)
;;   ("RET b" durand-mark-block)
;;   ("SPC RET f" counsel-describe-function)
;;   ("SPC RET k" describe-key)
;;   ("SPC RET v" counsel-describe-variable)
;;   ("SPC RET s" (cond
;;                 ((null durand-stop-timer)
;;                  (setf durand-stop-timer
;;                        (run-with-timer
;;                         (* (if (null current-prefix-arg)
;;                                60
;;                              (read-number "Quels minutes?" 60))
;;                            60)
;;                         nil
;;                         'durand-stop-reminder))
;;                  (list-timers))
;;                 ((timerp durand-stop-timer)
;;                  (cancel-timer durand-stop-timer)
;;                  (setf durand-stop-timer nil))
;;                 (t
;;                  (user-error "Unknown situation"))))
;;   ("RET e" durand-eval)
;;   ("SPC RET c" (cond (current-prefix-arg (durand-capture))
;;                      (t (org-capture))) :color blue)
;;   ("RET t" (if current-prefix-arg
;;                (make-process
;;                 :name "terminal"
;;                 :command '("open" "-a" "terminal")
;;                 :buffer nil)
;;              (eshell)))
;;   ("RET s" durand-open-browser)
;;   ("SPC :" dabbrev-completion)
;;   ("M-SPC" cycle-spacing)
;;   ("SPC a" evil-numbers/inc-at-pt)
;;   ("SPC A" evil-numbers/dec-at-pt)
;;   ("SPC =" align-regexp)
;;   ("SPC -" hydra--negative-argument)
;;   ("SPC p f" projectile-find-file)
;;   ("SPC p p" projectile-switch-project)
;;   ("SPC t" avy-goto-char-timer)
;;   ("SPC ," beginning-of-buffer)
;;   ("SPC m" durand-mu4e)
;;   ("SPC M" mu4e)
;;   ("SPC u" (if current-prefix-arg
;;                (org-store-link nil t)
;;              (mu4e-update-mail-and-index nil)))
;;   ("SPC ;" end-of-buffer)
;;   ("SPC é" split-window-below)
;;   ("SPC \"" split-window-right)
;;   ("SPC x" ace-swap-window)
;;   ("SPC w" delete-other-windows)
;;   ("SPC W" delete-window)
;;   ("SPC k" kill-other-buffer-window)
;;   ("SPC o" durand-new-buffer)
;;   ("SPC O" make-blank-space)
;;   ("f" counsel-find-file)
;;   ("SPC c" clean-up-buffers)
;;   ("SPC C" clean-up-buffers-regex)
;;   ("SPC j" durand-bookmark-jump-headlong :color blue)
;;   ("J" jump-to-other-window-link)
;;   ("SPC J" bookmark-set)
;;   ("SPC r" durand-recentf-jump-headlong)
;;   ("SPC g" backward-or-up-sexp)
;;   ("SPC h" forward-or-up-sexp)
;;   ("SPC s" durand-cap-sentence)
;;   ("RET RET" durand-open-link)
;;   ("RET g" durand-search-general-hydra)
;;   ("s-x" (setq display-line-numbers
;;                (and (null display-line-numbers)
;;                     'relative))))

;; (hydra-set-property 'general-hydra :verbosity 0)



;; ("u" (undo-tree-undo nil))
;; ("r" (undo-tree-redo nil))
;; ("A" (org-agenda))
;; ("l" (org-agenda nil "o") :color blue)
;; ("a" (org-agenda nil "a"))
;; ("t" (progn
;;        (find-file "/Users/durand/org/account/account.org")
;;        (org-account-go-to-last-day t)
;;        (org-columns)))
;; (",c" (org-capture))
;; (",u" (mu4e-update-mail-and-index nil))
;; ("'" (if (string= major-mode "org-mode")
;;          (org-edit-special)
;;        (message "Not in org mode buffer")) :color blue)
;; ("k" (durand-kill-buffer))
;; ("R" (durand-recentf-jump-headlong))
;; ("d" (call-interactively #'clean-up-reentf-list))
;; ("D" (setq recentf-list nil))
;; ("x" (amx))
;; ("t" (call-interactively 'avy-goto-char-timer))
;; ("s" (durand-general-save-buffer))
;; ("c" (clean-up-buffers))
;; ("C" (call-interactively #'clean-up-buffers-regex))
;; ("f" (counsel-find-file))
;; ("j" (durand-bookmark-jump-headlong) :color blue)
;; ("m" (bookmark-set))
;; (",m" (durand-mu4e))
;; (",M" (mu4e))
;; ("w" delete-other-windows)
;; ("SPC" (durand-buffer-scroll 'up) :color blue)
;; ("<backspace>" (durand-buffer-scroll 'down) :color blue)
;; ("v" (scroll-other-window '-))
;; ("n" (lambda (&optional arg)
;;        (interactive "p")
;;        (durand-general-move arg)))
;; ("p" (lambda (&optional arg)
;;        (interactive "p")
;;        (durand-general-move (* -1 arg))))
;; ("N" (lambda (&optional arg)
;;        (interactive "p")
;;        (durand-general-MOVE arg)))
;; ("P" (lambda (&optional arg)
;;        (interactive "p")
;;        (durand-general-MOVE (* -1 arg))))
;; (",n" (switch-to-buffer "*new*"))
;; ("e" (eshell))
;; ("é" split-window-below)
;; ("\"" split-window-right)
;; (")" (other-window 1))
;; ("O" (ace-select-window))
;; ("os" (scroll-other-window))
;; ("oS" (scroll-other-window '-))
;; ("ow" (kill-other-buffer-window))
;; ("o0" (delete-window))
;; ("S" (ace-swap-window))
;; ("=" (counsel-grep-or-swiper))
;; ("§" (vi-hydra/body) :color blue)
;; ("i" (insert-hydra/body) :color blue)

;; (define-prefix-command 'general-spc-map)
;; (define-prefix-command 'general-spc-ret-map)
;; (define-prefix-command 'general-ret-map)

;; (define-key general-hydra/keymap [?\ ] general-spc-map)
;; (define-key general-hydra/keymap [?\r] general-ret-map)

;; (define-key general-ret-map [?r] 'rectangle-mark-mode)
;; (define-key general-ret-map [?h] 'beginning-of-buffer)
;; (define-key general-ret-map [?g] 'end-of-buffer)
;; (define-key general-ret-map [?x] 'exchange-point-and-mark)
;; (define-key general-ret-map [? ] 'set-mark-command)


;; (define-key general-spc-map [?T] 'avy-goto-char-timer)
;; (define-key general-spc-map [?,] 'beginning-of-buffer)
;; (define-key general-spc-map [59] 'end-of-buffer) ; this is char ;
;; (define-key general-spc-map [?é] 'split-window-below)
;; (define-key general-spc-map [?\"] 'split-window-right)
;; (define-key general-spc-map [?x] 'ace-swap-window)
;; (define-key general-spc-map [?w] 'delete-other-windows)
;; (define-key general-spc-map [?W] 'delete-window)
;; (define-key general-spc-map [?k] 'durand-kill-buffer)
;; (define-key general-spc-map [?b] 'durand-switch-buffer)
;; (define-key general-spc-map [?n] 'durand-new-buffer)
;; (define-key general-spc-map [?f] 'counsel-find-file)
;; (define-key general-spc-map [?c] 'clean-up-buffers)
;; (define-key general-spc-map [?C] 'clean-up-buffers-regex)
;; (define-key general-spc-map [?j] 'durand-bookmark-jump-headlong)
;; (define-key general-spc-map [?r] 'durand-recentf-jump-headlong)
;; (define-key general-spc-map [?g] 'backward-sexp)
;; (define-key general-spc-map [?h] 'forward-sexp)
;; (define-key general-spc-map [?s] 'durand-general-save-buffer)
;; (define-key general-spc-map [? ] 'insert-hydra/body)
;; (define-key general-spc-map [?\d] 'insert-hydra/body)
;; (define-key general-spc-map [?\r] general-spc-ret-map)

;; (define-key general-spc-ret-map [?w] 'delete-window)
;; (define-key general-spc-ret-map [?d] (lambda ()
;;                                        "forward delete word"
;;                                        (interactive)
;;                                        (durand-general-kill 'forward 'word t)))
;; (define-key general-spc-ret-map [?D] (lambda ()
;;                                        "backward delete word"
;;                                        (interactive)
;;                                        (durand-general-kill 'backward 'word t)))
;; (define-key general-spc-ret-map [?v] (lambda ()
;;                                        "forward delete till end"
;;                                        (interactive)
;;                                        (durand-general-kill 'forward 'end t)))
;; (define-key general-spc-ret-map [?V] (lambda ()
;;                                        "backward delete till end"
;;                                        (interactive)
;;                                        (durand-general-kill 'backward 'end t)))
;; (define-key general-spc-ret-map [?f] (lambda ()
;;                                        "forward delete char-or-bracket"
;;                                        (interactive)
;;                                        (durand-general-kill 'forward 'char-or-bracket t)))
;; (define-key general-spc-ret-map [?F] (lambda ()
;;                                        "backward delete char-or-bracket"
;;                                        (interactive)
;;                                        (durand-general-kill 'backward 'char-or-bracket t)))

;; a forward-backward ring to store the last searched char
;; (defvar durand-vi-search-ring '()
;;   "A forward-backward ring to store the last searched char")

;; (defvar durand-vi-search-ring-max 16
;;   "The maximaum number to store in `durand-vi-search-ring'")

;; (defvar durand-vi-search-direction 'forward
;;   "Last searched direction in vi-hydra")

;; (defun durand-vi-search-forward-pos (n x)
;;   (interactive (let ((ch (read-char "Goto: "))
;; 		     (num (prefix-numeric-value current-prefix-arg)))
;; 		 (list num (char-to-string ch))))
;;   (let ((pos 0)
;; 	(end (line-end-position))
;; 	temp)
;;     (if (< (length durand-vi-search-ring) durand-vi-search-ring-max)
;; 	(push x durand-vi-search-ring)
;;       (setq durand-vi-search-ring (cons x (nbutlast durand-vi-search-ring))))
;;     (while (> n 0)
;;       (cl-decf n)
;;       (setq temp (string-match (regexp-quote x)
;; 			       (buffer-substring-no-properties
;; 				(+ (point) pos 1)
;; 				end)))
;;       (if (null temp)
;; 	  (setq n 0)
;; 	(setq pos (+ pos (match-end 0)))))
;;     (when (not (null temp))
;;       (+ (point) pos))))

;; (defun durand-vi-search-forward ()
;;   (interactive)
;;   (let ((pos (call-interactively 'durand-vi-search-forward-pos)))
;;     (when pos
;;       (setq durand-vi-search-direction 'forward)
;;       (goto-char pos))))

;; (defun durand-vi-search-forward-till ()
;;   (interactive)
;;   (let ((pos (call-interactively 'durand-vi-search-forward-pos)))
;;     (when pos
;;       (setq durand-vi-search-direction 'forward-till)
;;       (goto-char (1- pos)))))

;; (defun durand-vi-search-backward-pos (n x)
;;   (interactive (let ((ch (read-char "Goto: "))
;; 		     (num (prefix-numeric-value current-prefix-arg)))
;; 		 (list num (char-to-string ch))))
;;   (let* ((pos 0)
;; 	 (beg (line-beginning-position))
;; 	 temp)
;;     (if (< (length durand-vi-search-ring) durand-vi-search-ring-max)
;; 	(push x durand-vi-search-ring)
;;       (setq durand-vi-search-ring (cons x (nbutlast durand-vi-search-ring))))
;;     (while (> n 0)
;;       (cl-decf n)
;;       (setq temp (string-match (regexp-quote x)
;; 			       (reverse
;; 				(buffer-substring-no-properties
;; 				 beg
;; 				 (- (point) pos)))))
;;       (if (null temp)
;; 	  (setq n 0)
;; 	(setq pos (+ pos (match-beginning 0) 1))))
;;     (when (not (null temp))
;;       (- (point) pos))))

;; (defun durand-vi-search-backward ()
;;   (interactive)
;;   (let ((pos (call-interactively 'durand-vi-search-backward-pos)))
;;     (when pos
;;       (setq durand-vi-search-direction 'backward)
;;       (goto-char pos))))

;; (defun durand-vi-search-backward-till ()
;;   (interactive)
;;   (let ((pos (call-interactively 'durand-vi-search-backward-pos)))
;;     (when pos
;;       (setq durand-vi-search-direction 'backward-till)
;;       (goto-char (1+ pos)))))

;; (defun durand-search-same-direction ()
;;   (interactive)
;;   (let ((last-char (car durand-vi-search-ring)))
;;     (cond
;;      ((eq durand-vi-search-direction 'forward)
;;       (forward-char)
;;       (let ((pos (durand-vi-search-forward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char pos))
;; 	  (backward-char))))
;;      ((eq durand-vi-search-direction 'forward-till)
;;       (forward-char 2)
;;       (let ((pos (durand-vi-search-forward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char (1- pos)))
;; 	  (backward-char 2))))
;;      ((eq durand-vi-search-direction 'backward)
;;       (let ((pos (durand-vi-search-backward-pos 1 last-char)))
;; 	(when pos
;; 	  (progn
;; 	    (push-mark)
;; 	    (goto-char pos)))))
;;      ((eq durand-vi-search-direction 'backward-till)
;;       (backward-char)
;;       (let ((pos (durand-vi-search-backward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char (1+ pos)))
;; 	  (forward-char))))
;;      (t
;;       (error "the variable durand-vi-search-direction is wrong: %s" durand-vi-search-direction)))))

;; (defun durand-search-reverse-direction ()
;;   (interactive)
;;   (let ((last-char (car durand-vi-search-ring)))
;;     (cond
;;      ((eq durand-vi-search-direction 'forward)
;;       (backward-char)
;;       (let ((pos (durand-vi-search-backward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char pos))
;; 	  (forward-char))))
;;      ((eq durand-vi-search-direction 'forward-till)
;;       (when (eq (char-before) (aref last-char 0))
;; 	(backward-char))
;;       (let ((pos (durand-vi-search-backward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char (1+ pos)))
;; 	  (when (eq (char-after) (aref last-char 0))
;; 	    (forward-char)))))
;;      ((eq durand-vi-search-direction 'backward)
;;       (forward-char)
;;       (let ((pos (durand-vi-search-forward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char pos))
;; 	  (backward-char))))
;;      ((eq durand-vi-search-direction 'backward-till)
;;       (cond
;;        ((eq (char-after) (aref last-char 0))
;; 	(forward-char))
;;        ((eq (char-after (1+ (point))) (aref last-char 0))
;; 	(forward-char 2))
;;        (t
;; 	nil))
;;       (let ((pos (durand-vi-search-forward-pos 1 last-char)))
;; 	(if pos
;; 	    (progn
;; 	      (push-mark)
;; 	      (goto-char (1- pos)))
;; 	  (cond
;; 	   ((eq (char-before) (aref last-char 0))
;; 	    (backward-char))
;; 	   (t
;; 	    nil)))))
;;      (t
;;       (error "the variable durand-vi-search-direction is wrong: %s" durand-vi-search-direction)))))

;; (global-set-key [?\M-§] 'vi-hydra/body)
;; (defhydra vi-hydra (:hint nil :color amaranth
;; 			  :pre (progn
;; 				 (set-face-attribute 'durand-custom-mode-face nil :foreground "#bf8264")
;; 				 (setq durand-custom-modeline "V "))
;; 			  :post (progn
;; 				  (set-face-attribute 'durand-custom-mode-face nil :foreground "white")
;; 				  (setq durand-custom-modeline "")))
;;   "
;; vi hydra"
;;   ("h" backward-char)
;;   ("j" next-line)
;;   ("k" previous-line)
;;   ("l" forward-char)
;;   ("m" set-mark-command)
;;   ("à" beginning-of-line)
;;   ("$" end-of-line)
;;   ("/" comment-dwim)
;;   ("w" (progn
;; 	 (re-search-forward "\\W\\w")
;; 	 (backward-char)))
;;   ("W" (progn
;; 	 (re-search-forward "\\s-\\S-")
;; 	 (backward-char)))
;;   ("b" (progn
;; 	 (re-search-backward "\\W\\w")
;; 	 (forward-char)))
;;   ("B" (progn
;; 	 (re-search-backward "\\s-\\S-")
;; 	 (forward-char)))
;;   ("e" (progn
;; 	 (re-search-forward "\\w\\W")
;; 	 (backward-char)))
;;   ("E" (progn
;; 	 (re-search-forward "\\S-\\s-")
;; 	 (backward-char)))
;;   ("f" (durand-vi-search-forward))
;;   ("F" (durand-vi-search-backward))
;;   ("t" (durand-vi-search-forward-till))
;;   ("T" (durand-vi-search-backward-till))
;;   (";" (durand-search-same-direction))
;;   ("," (durand-search-reverse-direction))
;;   ("d" delete-region)
;;   ("s" kill-ring-save)
;;   ("y" yank)
;;   ("u" undo)
;;   ("SPC" (scroll-up))
;;   ("<backspace>" (scroll-down))
;;   ("§" (general-hydra/body) :color blue)
;;   ("v" (view-mode 1) :color blue)
;;   ("g" (beginning-of-buffer))
;;   ("G" (end-of-buffer))
;;   ;; ("H" (set-window-start nil (point)))
;;   ("H" (recenter 0))
;;   ;; ("M" (set-window-start nil (save-excursion
;;   ;;       		       (previous-line (/ (window-height) 2))
;;   ;;       		       (point))))
;;   ("M" (recenter (/ (window-body-height) 2)))
;;   ;; ("L" (set-window-start nil (save-excursion
;;   ;;       		       (previous-line (/ (* 5 (window-height)) 6))
;;   ;;       		       (point))))
;;   ("L" (recenter -3))
;;   ("x" (exchange-point-and-mark))
;;   ("a" (call-interactively 'avy-goto-char))
;;   ;; ("b" (call-interactively 'avy-goto-char-2))
;;   ("c" (call-interactively 'avy-goto-char-timer))
;;   ("o" (set-mark-command '(1)))
;;   ("i" (insert-hydra/body) :color blue)
;;   ("q" nil :color blue))

;; (hydra-set-property 'vi-hydra :verbosity 2)



;; In retrospect there is no need for this insert hydra...
;; (defhydra insert-hydra (:hint nil
;;                               :color pink
;;                               :pre (progn
;;                                      (set-face-attribute 'durand-custom-mode-face nil :foreground "#39bf4c")
;;                                      (setq durand-custom-modeline "I "))
;;                               :post (progn
;;                                       (set-face-attribute 'durand-custom-mode-face nil :foreground "white")
;;                                       (setq durand-custom-modeline "")))
;;   "
;; insert hydra"
;;   ("<home>" general-hydra/body :color blue))

;; (dolist (n '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-"))
;;   (define-key insert-hydra/keymap (kbd n) nil))

;; (hydra-set-property 'insert-hydra :verbosity 2)



;; iy hydra
;;;###autoload
;; (defhydra iy-hydra (:hint nil :color red
;;                             :pre (progn
;;                                    (set-face-attribute 'durand-custom-mode-face nil :foreground "white")
;;                                    (set-frame-parameter nil 'cursor-type 'hbar)
;;                                    (setf durand-custom-modeline
;;                                          (propertize "Y " 'help-echo "iy")))
;;                             :post (progn
;;                                     (set-face-attribute 'durand-custom-mode-face nil :foreground "#39bf4c")
;;                                     (set-frame-parameter nil 'cursor-type 'bar)
;;                                     (setq durand-custom-modeline
;;                                           (propertize "I " 'help-echo "spécifique"))))
;;   "
;; iy hydra"
;;   ("," iy-go-to-or-up-to-continue-backward)
;;   (";" iy-go-to-or-up-to-continue)
;;   ("f" iy-go-to-char)
;;   ("c" iy-go-to-char-backward)
;;   ("s-g" (message "iy"))
;;   ("<home>" (general-hydra/body) :color blue))

;; (hydra-set-property 'iy-hydra :verbosity 0)

;; (defhydra zoom-hydra (global-map "<f2>"
;; 				                         :pre (progn
;; 					                              (set-face-attribute 'durand-custom-mode-face nil :foreground "#bf8264")
;; 					                              (setq durand-custom-modeline "Z "))
;; 				                         :post (progn
;; 					                               (set-face-attribute 'durand-custom-mode-face nil :foreground "#39bf4c")
;; 					                               (setq durand-custom-modeline "I ")))
;;   "zoom"
;;   ("g" text-scale-increase "+")
;;   ("l" text-scale-decrease "-")
;;   ("q" nil "exit"))

(provide 'ideal)

;;; ideal.el ends here
