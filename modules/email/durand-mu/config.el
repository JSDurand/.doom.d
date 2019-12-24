;;; email/durand-mu/config.el -*- lexical-binding: t; -*-

(use-package! mu4e
  ;; :commands (durand-mu4e)
  :defer 3
  :config
  (setf mu4e-confirm-quit nil)
  (setq mu4e-maildir (expand-file-name "~/mbsync"))
  (setq mu4e-get-mail-command "mbsync gmail") ; mbsync works a lot better!
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-view-show-addresses t)     ; show full addresses!
  (setq mu4e-view-show-images t)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-use-fancy-chars t)
  ;; (setq mu4e-update-interval nil)
  (setq mu4e-completing-read-function 'ivy-completing-read)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
  (add-to-list
   'mu4e-bookmarks
   (make-mu4e-bookmark
    :name "week no trash no archive"
    :query "date:7d..now AND NOT maildir:/trash AND NOT maildir:/archive"
    :key ?d))

  (add-to-list
   'mu4e-bookmarks
   (make-mu4e-bookmark
    :name "week starred"
    :query "date:7d..now AND maildir:/suivis"
    :key ?s))

  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/archive" . ?a)
          ("/suivis" . ?s)
          ("/drafts" . ?d)))
  (map! :map mu4e-view-mode-map
        [?o] 'mu4e-view-attach-emacs
        [home] 'doom/escape)
  (add-to-list 'mu4e-view-actions
               '("Browse this mail" . mu4e-action-view-in-browser))

  (add-to-list 'mu4e-view-actions
               '("view attachment in pdf-view" . mu4e-view-attach-emacs))

  (remove '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions)
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Awllower"
            :enter-func (lambda () (mu4e-message "Entering Awllower context"))
            :leave-func (lambda () (mu4e-message "Leaving Awllower context"))
            ;; we match based on the contact-fields of the message
            :match-func (lambda (msg)
                          (when msg
                            (or
                             (mu4e-message-contact-field-matches msg :to "mmemmew@gmail.com")
                             (mu4e-message-contact-field-matches msg :from "mmemmew@gmail.com"))))
            :vars '((user-mail-address . "mmemmew@gmail.com")
                    (user-full-name . "李俊緯")
                    (mu4e-compose-signature . "#+BEGIN_EXPORT html
<span style=\"color:rgb(6,144,255)\">生 俊緯</span>
#+END_EXPORT")
                    (mu4e-sent-folder . "/gmail/sent")
                    (smtpmail-smtp-user . "mmemmew")
                    (smtpmail-local-domain . "gmail.com")
                    (smtpmail-default-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-service . 587)))
          (\, (make-mu4e-context
               :name "NCTS"
               :enter-func (lambda () (mu4e-message "Switch to the NCTS context"))
               :leave-func (lambda () (mu4e-message "Leave NCTS context"))
               ;; no leave-func
               ;; we match based on the maildir of the message
               ;; this matches maildir /Arkham and its sub-directories
               :match-func (lambda (msg)
                             (when msg
                               (or
                                (mu4e-message-contact-field-matches msg :to "chunweilee@ncts.ntu.edu.tw")
                                (mu4e-message-contact-field-matches msg :from "chunweilee@ncts.ntu.edu.tw"))))
               :vars (quote ((user-mail-address . "chunweilee@ncts.ntu.edu.tw")
                             (user-full-name . "李俊緯")
                             (mu4e-compose-signature .
                                                     (concat
                                                      "Sincerely Yours,\n"
                                                      "俊緯"))))))))
  (setq mu4e-context-policy 'pick-first
        mu4e-attachment-dir "~/Downloads"
        mu4e-mu-binary "/usr/local/bin/mu")
  ;; advice update function to log something.
  (advice-add 'mu4e-update-mail-and-index :after 'mu4e-update-advice)

  ;; doom for some weird reason does not want to delete mails, but I do.
  (setf mu4e-marks
        (push
         '(delete
           :char ("D" . "x")
           :prompt "Delete"
           :show-target (lambda (target) "delete")
           :action (lambda (docid msg target) (mu4e~proc-remove docid)))
         mu4e-marks))

  (defun mu4e-contacts-show-time (&rest args)
    "show time"
    (mu4e-index-message "Temps: %s, contacts: %d"
                        (format-time-string "%k:%M:%S")
                        (hash-table-count mu4e~contacts)))


  (advice-add 'mu4e-info-handler :around 'mu4e-update-show-time)
  (advice-add 'mu4e~fill-contacts :after 'mu4e-contacts-show-time))

(use-package! mu4e-alert
  :defer 3
  :config
  (setf mu4e-alert-interesting-mail-query
        "date:7d..now AND NOT maildir:/trash AND NOT maildir:/archive AND flag:unread"
        mu4e-update-interval 3600)
  (mu4e-alert-enable-mode-line-display)

;;;###autoload
  (defun durand-mu4e (&optional arg)
    "If there are unread mails, view them; else, show the time until the next update,
unless called multiple times, in which case execute `mu4e'.
With ARG, toggle mu4e.
If mu4e is not turned on, then tell the user this fact."
    (interactive "P")
    (cond
     ((and arg (or (get-process " *mu4e-proc*")
                   mu4e~update-timer))
      (mu4e-quit)
      (message "mu4e is turned off now."))
     (arg
      (mu4e))
     ((> mu4e-alert-mode-line 0) (mu4e-alert-view-unread-mails))
     ;; ((eq last-command 'durand-mu4e) (mu4e))
     ((and (memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
           mu4e~update-timer)
      (mu4e)
      (mu4e-next-update-seconds))
     ((memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
      (mu4e)
      (setq mu4e~update-timer
            (run-at-time
             0 mu4e-update-interval
             (lambda () (mu4e-update-mail-and-index
                         mu4e-index-update-in-background)))))
     ((and (get-process " *mu4e-proc*")
           mu4e~update-timer)
      (mu4e-next-update-seconds))
     ((or (and (not (get-process " *mu4e-proc*"))
               mu4e~update-timer)
          (and (get-process " *mu4e-proc*")
               (null mu4e~update-timer)))
      (mu4e-quit)
      (user-error "Fixé une erreur."))
     (t
      (message "mu4e is not active right now."))))

;;;###autoload
  (defun durand-mu4e-format-string (&rest args)
    "Format string"
    (interactive)
    (let* ((args (-filter (lambda (ele) (/= (car ele) 0)) args))
           (derniere (last args))
           (except-last (reverse (cdr (reverse args))))
           (number (caar derniere))
           (str (cadar derniere))
           (number-face '(:foreground "DeepSkyBlue1"))
           (str-face '(:foreground "lightskyblue"))
           res)
      (dolist (var except-last)
        (when (listp var)
          (let ((number (car var))
                (str (cadr var)))
            (setf res (concat
                       res
                       (format " %s %s"
                               (propertize (format "%d" number) 'face number-face)
                               (propertize (if (> number 1) (concat str "s") str)
                                           'face str-face))
                       (if (>= (length args) 3) "," ""))))))
      (setf res (concat
                 (when res (substring res 1))
                 (if (<= (length args) 1)
                     ""
                   " et ")
                 (format "%s %s avant le prochain mis-à-jour."
                         (propertize (format "%d" number) 'face number-face)
                         (propertize (if (> number 1) (concat str "s") str)
                                     'face str-face))))))

;;;###autoload
  (defun mu4e-next-update-seconds ()
    "Return the number of seconds before the next automatic update"
    (interactive)
    (let* ((time (time-subtract `(,(aref mu4e~update-timer 1) ,(aref mu4e~update-timer 2)
                                  ,(aref mu4e~update-timer 3) ,(aref mu4e~update-timer 4))
                                (current-time)))
           (total-secs (truncate (time-to-seconds time)))
           (days (/ total-secs 86400))
           (hours (% (/ total-secs 3600) 24))
           (mins (% (/ total-secs 60) 60))
           (secs (% total-secs 60))
           (str (durand-mu4e-format-string `(,days "jour")
                                           `(,hours "heure")
                                           `(,mins "min")
                                           `(,secs "sec")))
           (update-info (if (get-process "mu4e-update") "Updating, " "")))
      (mu4e-message "%s%s" update-info str)))

;;;###autoload
  (defun mu4e-update-show-time (oldfun &rest info)
    (if (and
         (eq (plist-get (car info) :info) 'index)
         (not (eq (plist-get (car info) :status) 'running)))
        (progn
          (mu4e-index-message
           "Time: %s; Indexing completed; processed %d, updated %d, cleaned-up %d"
           (format-time-string "%k:%M:%S")
           (plist-get (car info) :processed) (plist-get (car info) :updated)
           (plist-get (car info) :cleaned-up))
          (unless (or (zerop (plist-get (car info) :updated)) (not mu4e~contacts))
            (mu4e~request-contacts-maybe)
            (run-hooks 'mu4e-index-updated-hook)))
      (apply oldfun info)))

  ;; redefine this search function
;;;###autoload
  (defun mu4e~headers-search-execute (expr ignore-history)
    "Search in the mu database for EXPR, and switch to the output
buffer for the results. If IGNORE-HISTORY is true, do *not* update
the query history stack."
    ;; note: we don't want to update the history if this query comes from
    ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'.
    ;;(mu4e-hide-other-mu4e-buffers)
    (let* ((buf (get-buffer-create mu4e~headers-buffer-name))
           (inhibit-read-only t)
           (rewritten-expr (funcall mu4e-query-rewrite-function expr))
           (maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit)))
      (with-current-buffer buf
        (mu4e-headers-mode)
        (unless ignore-history
          ;; save the old present query to the history list
          (when mu4e~headers-last-query
            (mu4e~headers-push-query mu4e~headers-last-query 'past)))
        (setq
         ;; don't set mode-name, s'il vous plaît
         ;; mode-name "mu4e-headers"
         mu4e~headers-last-query rewritten-expr)
        (add-to-list 'global-mode-string
                     '(:eval
                       (concat
                        (propertize
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         'face 'mu4e-modeline-face)
                        " "
                        (mu4e-context-label)
                        (if (and mu4e-display-update-status-in-modeline
                                 (buffer-live-p mu4e~update-buffer)
                                 (process-live-p (get-buffer-process
                                                  mu4e~update-buffer)))
                            (propertize " (updating)" 'face 'mu4e-modeline-face)
                          "")))))

      ;; when the buffer is already visible, select it; otherwise,
      ;; switch to it.
      (unless (get-buffer-window buf 0)
        (switch-to-buffer buf))
      (run-hook-with-args 'mu4e-headers-search-hook expr)
      (mu4e~headers-clear mu4e~searching)
      (mu4e~proc-find
       rewritten-expr
       mu4e-headers-show-threads
       mu4e-headers-sort-field
       mu4e-headers-sort-direction
       maxnum
       mu4e-headers-skip-duplicates
       mu4e-headers-include-related))))


(after! mu4e
  ;; Je ne veux pas quitter mu4e quand je touche "q".
  (map! :map mu4e-main-mode-map
        :n [?q] 'bury-buffer
        :n [?u] 'mu4e-update-mail-and-index)
  (add-hook! mu4e-main-mode :append 'mu4e-clear-quit))

(after! evil
  (set-evil-initial-state!
    '(mu4e-main-mode
      mu4e-compose-mode
      mu4e~update-mail-mode)
    'normal)

  (set-evil-initial-state!
    '(mu4e-headers-mode
      mu4e-view-mode)
    'emacs))

(setf durand-mu4e-open-timer
      (unless (boundp 'durand-mu4e-open-timer)
        (let* ((cur (decode-time (current-time)))
               (cur-year (nth 5 cur))
               (cur-month (nth 4 cur))
               (cur-day (nth 3 cur))
               (cur-hour (nth 2 cur)))
          (run-with-timer
           (float-time
            (time-subtract
             (cond
              ((>= cur-hour 9)
               (encode-time 0 0 9 (1+ cur-day) cur-month cur-year))
              (t
               (encode-time 0 0 9 cur-day cur-month cur-year)))
             nil))
           (* 24 60 60) ;; a day
           #'durand-mu4e-open-if-necessary)))
      durand-mu4e-close-timer
      (unless (boundp 'durand-mu4e-close-timer)
        (let* ((cur (decode-time (current-time)))
               (cur-year (nth 5 cur))
               (cur-month (nth 4 cur))
               (cur-day (nth 3 cur))
               (cur-hour (nth 2 cur)))
          (run-with-timer
           (float-time
            (time-subtract
             (cond
              ((>= cur-hour 22)
               (encode-time 0 0 22 (1+ cur-day) cur-month cur-year))
              (t
               (encode-time 0 0 22 cur-day cur-month cur-year)))
             nil))
           (* 24 60 60) ;; a day
           #'durand-mu4e-close-if-necessary))))

;; ARCHIVE
;; (\, (make-mu4e-context
;;                :name "BaoBao"
;;                :enter-func (lambda () (mu4e-message "Entering 寶寶 context"))
;;                :leave-func (lambda () (mu4e-message "Leaving 寶寶 context"))
;;                ;; we match based on the contact-fields of the message
;;                :match-func (lambda (msg)
;;                              (when msg
;;                                (or
;;                                 (mu4e-message-contact-field-matches msg :to "lintingtsen@gmail.com")
;;                                 (mu4e-message-contact-field-matches msg :from "lintingtsen@gmail.com"))))
;;                :vars (quote ((user-mail-address . "mmemmew@gmail.com")
;;                              (user-full-name . "大寶寶")
;;                              (mu4e-compose-signature .
;;                                                      "愛你的寶寶")
;;                              (mu4e-sent-folder . "/gmail/sent")
;;                              (smtpmail-smtp-user . "mmemmew")
;;                              (smtpmail-local-domain . "gmail.com")
;;                              (smtpmail-default-smtp-server . "smtp.gmail.com")
;;                              (smtpmail-smtp-server . "smtp.gmail.com")
;;                              (smtpmail-smtp-service . 587)))))
