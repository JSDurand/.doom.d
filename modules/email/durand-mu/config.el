;;; email/durand-mu/config.el -*- lexical-binding: t; -*-

(use-package! mu4e
  :after-call (durand-mu4e)
  ;; :defer 3
  :config
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  (require 'org-mu4e)
  (require 'message)
  (setf message-confirm-send t)


  (remove-hook 'mu4e-view-mode-hook #'evil-emacs-state)

  (setq mu4e-org-link-query-in-headers-mode nil)
  (setf mu4e~update-buffer-height 5)

;;; REVIEW: This is done by default in the new version of mu4e
  ;; (org-link-set-parameters
  ;;  "mu4e"
  ;;  :follow #'org-mu4e-open
  ;;  :store #'org-mu4e-store-link)

  (setf mu4e-view-use-gnus t)
  (setf mu4e-confirm-quit nil)
  ;; NOTE: This is determined by the server now.
  ;; (setq mu4e-maildir (expand-file-name "~/mbsync"))
  (setq mu4e-get-mail-command "mbsync -a") ; mbsync works a lot better!
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
        :n [?o] 'mu4e-view-attach-emacs
        :n [?!] (cmd! (evil-scroll-line-down 1))
        [escape] 'doom/escape)
  (add-to-list 'mu4e-view-actions
               '("Browse this mail" . mu4e-action-view-in-browser))

  (add-to-list 'mu4e-view-actions
               '("view attachment in pdf-view" . mu4e-view-attach-emacs))

  ;; (remove '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Student"
          :enter-func (lambda () (mu4e-message "Entering Student context"))
          :leave-func (lambda () (mu4e-message "Leaving Student context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches msg :from "tan\\|mlh\\|hsialc\\|tingyu.lee\\|tingyulee")
                           (mu4e-message-contact-field-matches msg :to "tan\\|mlh\\|hsialc\\|tingyu.lee\\|tingyulee"))))
          :vars '((user-mail-address . "mmemmew@gmail.com")
                  (user-full-name . "李俊緯")
                  (mu4e-compose-signature . "生 俊緯")
                  (mu4e-sent-folder . "/gmail/sent")
                  (smtpmail-smtp-user . "mmemmew")
                  (smtpmail-local-domain . "gmail.com")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))
         (make-mu4e-context
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
          :vars '((user-mail-address . "chunweilee@ncts.ntu.edu.tw")
                  (user-full-name . "李俊緯")
                  (mu4e-compose-signature .
                                          (concat
                                           "Sincerely Yours,\n"
                                           "俊緯"))))
         (make-mu4e-context
          :name "Durand"
          :enter-func (lambda () (mu4e-message "Entering Durand context"))
          :leave-func (lambda () (mu4e-message "Leaving Durand context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches msg :to "mmemmew@gmail.com")
                           (mu4e-message-contact-field-matches msg :from "mmemmew@gmail.com"))))
          :vars '((user-mail-address . "mmemmew@gmail.com")
                  (user-full-name . "Durand")
                  (mu4e-compose-signature . "Sévère Durand")
                  (mu4e-sent-folder . "/gmail/sent")
                  (smtpmail-smtp-user . "mmemmew")
                  (smtpmail-local-domain . "gmail.com")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))))
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


  (advice-add 'mu4e-info-handler :around 'mu4e-update-show-time)
  (advice-add 'mu4e~fill-contacts :after 'mu4e-contacts-show-time))

(use-package! mu4e-alert
  :after mu4e
  :config
  (setf mu4e-alert-interesting-mail-query
        "date:7d..now AND NOT maildir:/trash AND NOT maildir:/archive AND flag:unread"
        mu4e-update-interval nil)
  (mu4e-alert-enable-mode-line-display))

(use-package! org-mu4e
  :hook (mu4e-compose-mode . org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-convert-to-html nil)
  (when (version< mu4e-mu-version "1.4")
    (setq org-mu4e-link-query-in-headers-mode nil))

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  (setq-hook! 'message-send-hook org-mu4e-convert-to-html nil))

(after! mu4e
  ;; Je ne veux pas quitter mu4e quand je touche "q".
  (map! :map mu4e-main-mode-map
        :n [?q] 'bury-buffer
        :n [?u] 'mu4e-update-mail-and-index
        :map mu4e-view-mode-map
        :n [tab] 'durand-forward-link
        :n [backtab] 'durand-backward-link
        :mode 'mu4e-view-mode
        :localleader
        :n [?n] 'mu4e-view-headers-next
        :n [?p] 'mu4e-view-headers-prev)
  (add-hook! mu4e-main-mode :append 'mu4e-clear-quit)

  ;; set fill-columns properly
  (add-hook! 'mu4e-compose-mode-hook :append
    (setq-local durand-org-fill-column 70))

  (map! :map mu4e-view-mode-map
        :n [?o] 'mu4e-view-attach-emacs
        :n [?!] (cmd! (evil-scroll-line-down 1))
        [escape] 'doom/escape
        :map mu4e-headers-mode-map
        :n [?!] (cmd! (evil-scroll-line-down 1))
        :n [?\M-n] 'durand-mu4e-next-thread
        :n [?\M-p] 'durand-mu4e-prev-thread
        :leader
        [?!] 'mu4e-headers-mark-for-read))

(after! evil
  (set-evil-initial-state!
    '(mu4e-main-mode
      mu4e-compose-mode
      mu4e~update-mail-mode)
    'normal)

  ;; (set-evil-initial-state!
  ;;   '(mu4e-headers-mode
  ;;     mu4e-view-mode)
  ;;   'emacs)
  )

;; (setf durand-mu4e-open-timer
;;       (unless (boundp 'durand-mu4e-open-timer)
;;         (let* ((cur (decode-time (current-time)))
;;                (cur-year (nth 5 cur))
;;                (cur-month (nth 4 cur))
;;                (cur-day (nth 3 cur))
;;                (cur-hour (nth 2 cur)))
;;           (run-with-timer
;;            (float-time
;;             (time-subtract
;;              (cond
;;               ((>= cur-hour 9)
;;                (encode-time 0 0 9 (1+ cur-day) cur-month cur-year))
;;               (t
;;                (encode-time 0 0 9 cur-day cur-month cur-year)))
;;              nil))
;;            (* 24 60 60) ;; a day
;;            #'durand-mu4e-open-if-necessary)))
;;       durand-mu4e-close-timer
;;       (unless (boundp 'durand-mu4e-close-timer)
;;         (let* ((cur (decode-time (current-time)))
;;                (cur-year (nth 5 cur))
;;                (cur-month (nth 4 cur))
;;                (cur-day (nth 3 cur))
;;                (cur-hour (nth 2 cur)))
;;           (run-with-timer
;;            (float-time
;;             (time-subtract
;;              (cond
;;               ((>= cur-hour 22)
;;                (encode-time 0 0 22 (1+ cur-day) cur-month cur-year))
;;               (t
;;                (encode-time 0 0 22 cur-day cur-month cur-year)))
;;              nil))
;;            (* 24 60 60) ;; a day
;;            #'durand-mu4e-close-if-necessary))))

;;; org-msg

;; (use-package! org-msg
;;   :after mu4e
;;   :config
;;   )


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
