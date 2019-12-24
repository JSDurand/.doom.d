;;; email/durand-mu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mu4e-clear-quit ()
  "Don't quit when I touch \"q\"."
  (map! :map mu4e-main-mode-map
        :n [?q] 'bury-buffer)
  (remove-hook 'mu4e-main-mode-hook 'mu4e-clear-quit))

;; (use-package! mu4e
;;   :defer 3
;;   :config)

;;;###autoload
(defun mu4e-view-attach-emacs (n)
  "Open the N-th attachment in emacs"
  (interactive (list (read-number "Enter attachment number: " 1)))
  (mu4e-view-open-attachment-emacs (mu4e-message-at-point) n))

;;;###autoload
(defun durand-mu4e-open-if-necessary ()
  "If `mu4e' is not already open, then open it."
  (cond
   ((get-process " *mu4e-proc*")
    (message "déjà ouvert"))
   (t
    (mu4e)
    (switch-to-buffer (other-buffer))
    (message "ouvert maintenant"))))

;;;###autoload
(defun durand-mu4e-close-if-necessary ()
  "If `mu4e' is open, then close it."
  (cond
   ((get-process " *mu4e-proc*")
    (mu4e-quit)
    (message "fermé maintenant"))
   (t
    (message "déjà fermé"))))

;; (use-package! mu4e-alert
;;   :defer 3
;;   :config

;; ;;;###autoload
;;   (defun durand-mu4e (&optional arg)
;;     "If there are unread mails, view them; else, show the time until the next update,
;; unless called multiple times, in which case execute `mu4e'.
;; With ARG, toggle mu4e.
;; If mu4e is not turned on, then tell the user this fact."
;;     (interactive "P")
;;     (cond
;;      ((and arg (or (get-process " *mu4e-proc*")
;;                    mu4e~update-timer))
;;       (mu4e-quit)
;;       (message "mu4e is turned off now."))
;;      (arg
;;       (mu4e))
;;      ((> mu4e-alert-mode-line 0) (mu4e-alert-view-unread-mails))
;;      ;; ((eq last-command 'durand-mu4e) (mu4e))
;;      ((and (memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
;;            mu4e~update-timer)
;;       (mu4e)
;;       (mu4e-next-update-seconds))
;;      ((memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
;;       (mu4e)
;;       (setq mu4e~update-timer
;;             (run-at-time
;;              0 mu4e-update-interval
;;              (lambda () (mu4e-update-mail-and-index
;;                          mu4e-index-update-in-background)))))
;;      ((and (get-process " *mu4e-proc*")
;;            mu4e~update-timer)
;;       (mu4e-next-update-seconds))
;;      ((or (and (not (get-process " *mu4e-proc*"))
;;                mu4e~update-timer)
;;           (and (get-process " *mu4e-proc*")
;;                (null mu4e~update-timer)))
;;       (mu4e-quit)
;;       (user-error "Fixé une erreur."))
;;      (t
;;       (message "mu4e is not active right now."))))

;; ;;;###autoload
;;   (defun durand-mu4e-format-string (&rest args)
;;     "Format string"
;;     (interactive)
;;     (let* ((args (-filter (lambda (ele) (/= (car ele) 0)) args))
;;            (derniere (last args))
;;            (except-last (reverse (cdr (reverse args))))
;;            (number (caar derniere))
;;            (str (cadar derniere))
;;            (number-face '(:foreground "DeepSkyBlue1"))
;;            (str-face '(:foreground "lightskyblue"))
;;            res)
;;       (dolist (var except-last)
;;         (when (listp var)
;;           (let ((number (car var))
;;                 (str (cadr var)))
;;             (setf res (concat
;;                        res
;;                        (format " %s %s"
;;                                (propertize (format "%d" number) 'face number-face)
;;                                (propertize (if (> number 1) (concat str "s") str)
;;                                            'face str-face))
;;                        (if (>= (length args) 3) "," ""))))))
;;       (setf res (concat
;;                  (when res (substring res 1))
;;                  (if (<= (length args) 1)
;;                      ""
;;                    " et ")
;;                  (format "%s %s avant le prochain mis-à-jour."
;;                          (propertize (format "%d" number) 'face number-face)
;;                          (propertize (if (> number 1) (concat str "s") str)
;;                                      'face str-face))))))

;; ;;;###autoload
;;   (defun mu4e-next-update-seconds ()
;;     "Return the number of seconds before the next automatic update"
;;     (interactive)
;;     (let* ((time (time-subtract `(,(aref mu4e~update-timer 1) ,(aref mu4e~update-timer 2)
;;                                   ,(aref mu4e~update-timer 3) ,(aref mu4e~update-timer 4))
;;                                 (current-time)))
;;            (total-secs (truncate (time-to-seconds time)))
;;            (days (/ total-secs 86400))
;;            (hours (% (/ total-secs 3600) 24))
;;            (mins (% (/ total-secs 60) 60))
;;            (secs (% total-secs 60))
;;            (str (durand-mu4e-format-string `(,days "jour")
;;                                            `(,hours "heure")
;;                                            `(,mins "min")
;;                                            `(,secs "sec")))
;;            (update-info (if (get-process "mu4e-update") "Updating, " "")))
;;       (mu4e-message "%s%s" update-info str)))

;; ;;;###autoload
;;   (defun mu4e-update-show-time (oldfun &rest info)
;;     (if (and
;;          (eq (plist-get (car info) :info) 'index)
;;          (not (eq (plist-get (car info) :status) 'running)))
;;         (progn
;;           (mu4e-index-message
;;            "Time: %s; Indexing completed; processed %d, updated %d, cleaned-up %d"
;;            (format-time-string "%k:%M:%S")
;;            (plist-get (car info) :processed) (plist-get (car info) :updated)
;;            (plist-get (car info) :cleaned-up))
;;           (unless (or (zerop (plist-get (car info) :updated)) (not mu4e~contacts))
;;             (mu4e~request-contacts-maybe)
;;             (run-hooks 'mu4e-index-updated-hook)))
;;       (apply oldfun info)))

;; ;;;###autoload
;;   (defun mu4e-contacts-show-time (&rest args)
;;     "show time"
;;     (mu4e-index-message "Temps: %s, contacts: %d"
;;                         (format-time-string "%k:%M:%S")
;;                         (hash-table-count mu4e~contacts)))

;;   (advice-add 'mu4e-info-handler :around 'mu4e-update-show-time)
;;   (advice-add 'mu4e~fill-contacts :after 'mu4e-contacts-show-time)

;;   ;; redefine this search function
;; ;;;###autoload
;;   (defun mu4e~headers-search-execute (expr ignore-history)
;;     "Search in the mu database for EXPR, and switch to the output
;; buffer for the results. If IGNORE-HISTORY is true, do *not* update
;; the query history stack."
;;     ;; note: we don't want to update the history if this query comes from
;;     ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'.
;;     ;;(mu4e-hide-other-mu4e-buffers)
;;     (let* ((buf (get-buffer-create mu4e~headers-buffer-name))
;;            (inhibit-read-only t)
;;            (rewritten-expr (funcall mu4e-query-rewrite-function expr))
;;            (maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit)))
;;       (with-current-buffer buf
;;         (mu4e-headers-mode)
;;         (unless ignore-history
;;           ;; save the old present query to the history list
;;           (when mu4e~headers-last-query
;;             (mu4e~headers-push-query mu4e~headers-last-query 'past)))
;;         (setq
;;          ;; don't set mode-name, s'il vous plaît
;;          ;; mode-name "mu4e-headers"
;;          mu4e~headers-last-query rewritten-expr)
;;         (add-to-list 'global-mode-string
;;                      '(:eval
;;                        (concat
;;                         (propertize
;;                          (mu4e~quote-for-modeline mu4e~headers-last-query)
;;                          'face 'mu4e-modeline-face)
;;                         " "
;;                         (mu4e-context-label)
;;                         (if (and mu4e-display-update-status-in-modeline
;;                                  (buffer-live-p mu4e~update-buffer)
;;                                  (process-live-p (get-buffer-process
;;                                                   mu4e~update-buffer)))
;;                             (propertize " (updating)" 'face 'mu4e-modeline-face)
;;                           "")))))

;;       ;; when the buffer is already visible, select it; otherwise,
;;       ;; switch to it.
;;       (unless (get-buffer-window buf 0)
;;         (switch-to-buffer buf))
;;       (run-hook-with-args 'mu4e-headers-search-hook expr)
;;       (mu4e~headers-clear mu4e~searching)
;;       (mu4e~proc-find
;;        rewritten-expr
;;        mu4e-headers-show-threads
;;        mu4e-headers-sort-field
;;        mu4e-headers-sort-direction
;;        maxnum
;;        mu4e-headers-skip-duplicates
;;        mu4e-headers-include-related)))

;;   )

;;;###autoload
(defun mu4e-update-advice (run-in-background)
  "Log it after the update."
  (if run-in-background
      (message "Update in background finished.")
    (message "Update finished.")))

;;;###autoload
(defun mu4e-contacts-show-time (&rest args)
  "show time"
  (after! mu4e
    (mu4e-index-message "Temps: %s, contacts: %d"
                        (format-time-string "%k:%M:%S")
                        (hash-table-count mu4e~contacts))))

;;;###autoload
(defun durand-mu4e (&optional arg)
  "If there are unread mails, view them; else, show the time until the next update,
unless called multiple times, in which case execute `mu4e'.
With ARG, toggle mu4e.
If mu4e is not turned on, then tell the user this fact."
  (interactive "P")
  (require 'mu4e)
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
    (after! mu4e (mu4e-message "%s%s" update-info str))))

;;;###autoload
(defun mu4e-update-show-time (oldfun &rest info)
  (require 'mu4e)
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
;; (defun mu4e~headers-search-execute (expr ignore-history)
;;   "Search in the mu database for EXPR, and switch to the output
;; buffer for the results. If IGNORE-HISTORY is true, do *not* update
;; the query history stack."
;;   ;; NOTE: We don't want to update the history if this query comes from
;;   ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'.
;;   ;;(mu4e-hide-other-mu4e-buffers)
;;   (let* ((buf (get-buffer-create mu4e~headers-buffer-name))
;;          (inhibit-read-only t)
;;          (rewritten-expr (funcall mu4e-query-rewrite-function expr))
;;          (maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit)))
;;     (with-current-buffer buf
;;       (mu4e-headers-mode)
;;       (unless ignore-history
;;         ;; save the old present query to the history list
;;         (when mu4e~headers-last-query
;;           (mu4e~headers-push-query mu4e~headers-last-query 'past)))
;;       (setq
;;        ;; don't set mode-name, s'il vous plaît
;;        ;; mode-name "mu4e-headers"
;;        mu4e~headers-last-query rewritten-expr)
;;       (add-to-list 'global-mode-string
;;                    '(:eval
;;                      (concat
;;                       (propertize
;;                        (mu4e~quote-for-modeline mu4e~headers-last-query)
;;                        'face 'mu4e-modeline-face)
;;                       " "
;;                       (mu4e-context-label)
;;                       (if (and mu4e-display-update-status-in-modeline
;;                                (buffer-live-p mu4e~update-buffer)
;;                                (process-live-p (get-buffer-process
;;                                                 mu4e~update-buffer)))
;;                           (propertize " (updating)" 'face 'mu4e-modeline-face)
;;                         "")))))

;;     ;; when the buffer is already visible, select it; otherwise,
;;     ;; switch to it.
;;     (unless (get-buffer-window buf 0)
;;       (switch-to-buffer buf))
;;     (run-hook-with-args 'mu4e-headers-search-hook expr)
;;     (mu4e~headers-clear mu4e~searching)
;;     (mu4e~proc-find
;;      rewritten-expr
;;      mu4e-headers-show-threads
;;      mu4e-headers-sort-field
;;      mu4e-headers-sort-direction
;;      maxnum
;;      mu4e-headers-skip-duplicates
;;      mu4e-headers-include-related)))

;; (after! mu4e-alert
;; ;;;###autoload
;;   (defun durand-mu4e (&optional arg)
;;     "If there are unread mails, view them; else, show the time until the next update,
;; unless called multiple times, in which case execute `mu4e'.
;; With ARG, toggle mu4e.
;; If mu4e is not turned on, then tell the user this fact."
;;     (interactive "P")
;;     (cond
;;      ((and arg (or (get-process " *mu4e-proc*")
;;                    mu4e~update-timer))
;;       (mu4e-quit)
;;       (message "mu4e is turned off now."))
;;      (arg
;;       (mu4e))
;;      ((> mu4e-alert-mode-line 0) (mu4e-alert-view-unread-mails))
;;      ;; ((eq last-command 'durand-mu4e) (mu4e))
;;      ((and (memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
;;            mu4e~update-timer)
;;       (mu4e)
;;       (mu4e-next-update-seconds))
;;      ((memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
;;       (mu4e)
;;       (setq mu4e~update-timer
;;             (run-at-time
;;              0 mu4e-update-interval
;;              (lambda () (mu4e-update-mail-and-index
;;                          mu4e-index-update-in-background)))))
;;      ((and (get-process " *mu4e-proc*")
;;            mu4e~update-timer)
;;       (mu4e-next-update-seconds))
;;      ((or (and (not (get-process " *mu4e-proc*"))
;;                mu4e~update-timer)
;;           (and (get-process " *mu4e-proc*")
;;                (null mu4e~update-timer)))
;;       (mu4e-quit)
;;       (user-error "Fixé une erreur."))
;;      (t
;;       (message "mu4e is not active right now."))))

;; ;;;###autoload
;;   (defun durand-mu4e-format-string (&rest args)
;;     "Format string"
;;     (interactive)
;;     (let* ((args (-filter (lambda (ele) (/= (car ele) 0)) args))
;;            (derniere (last args))
;;            (except-last (reverse (cdr (reverse args))))
;;            (number (caar derniere))
;;            (str (cadar derniere))
;;            (number-face '(:foreground "DeepSkyBlue1"))
;;            (str-face '(:foreground "lightskyblue"))
;;            res)
;;       (dolist (var except-last)
;;         (when (listp var)
;;           (let ((number (car var))
;;                 (str (cadr var)))
;;             (setf res (concat
;;                        res
;;                        (format " %s %s"
;;                                (propertize (format "%d" number) 'face number-face)
;;                                (propertize (if (> number 1) (concat str "s") str)
;;                                            'face str-face))
;;                        (if (>= (length args) 3) "," ""))))))
;;       (setf res (concat
;;                  (when res (substring res 1))
;;                  (if (<= (length args) 1)
;;                      ""
;;                    " et ")
;;                  (format "%s %s avant le prochain mis-à-jour."
;;                          (propertize (format "%d" number) 'face number-face)
;;                          (propertize (if (> number 1) (concat str "s") str)
;;                                      'face str-face))))))

;; ;;;###autoload
;;   (defun mu4e-next-update-seconds ()
;;     "Return the number of seconds before the next automatic update"
;;     (interactive)
;;     (let* ((time (time-subtract `(,(aref mu4e~update-timer 1) ,(aref mu4e~update-timer 2)
;;                                   ,(aref mu4e~update-timer 3) ,(aref mu4e~update-timer 4))
;;                                 (current-time)))
;;            (total-secs (truncate (time-to-seconds time)))
;;            (days (/ total-secs 86400))
;;            (hours (% (/ total-secs 3600) 24))
;;            (mins (% (/ total-secs 60) 60))
;;            (secs (% total-secs 60))
;;            (str (durand-mu4e-format-string `(,days "jour")
;;                                            `(,hours "heure")
;;                                            `(,mins "min")
;;                                            `(,secs "sec")))
;;            (update-info (if (get-process "mu4e-update") "Updating, " "")))
;;       (mu4e-message "%s%s" update-info str)))

;; ;;;###autoload
;;   (defun mu4e-update-show-time (oldfun &rest info)
;;     (if (and
;;          (eq (plist-get (car info) :info) 'index)
;;          (not (eq (plist-get (car info) :status) 'running)))
;;         (progn
;;           (mu4e-index-message
;;            "Time: %s; Indexing completed; processed %d, updated %d, cleaned-up %d"
;;            (format-time-string "%k:%M:%S")
;;            (plist-get (car info) :processed) (plist-get (car info) :updated)
;;            (plist-get (car info) :cleaned-up))
;;           (unless (or (zerop (plist-get (car info) :updated)) (not mu4e~contacts))
;;             (mu4e~request-contacts-maybe)
;;             (run-hooks 'mu4e-index-updated-hook)))
;;       (apply oldfun info)))


;;   ;; redefine this search function
;; ;;;###autoload
;;   (defun mu4e~headers-search-execute (expr ignore-history)
;;     "Search in the mu database for EXPR, and switch to the output
;; buffer for the results. If IGNORE-HISTORY is true, do *not* update
;; the query history stack."
;;     ;; note: we don't want to update the history if this query comes from
;;     ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'.
;;     ;;(mu4e-hide-other-mu4e-buffers)
;;     (let* ((buf (get-buffer-create mu4e~headers-buffer-name))
;;            (inhibit-read-only t)
;;            (rewritten-expr (funcall mu4e-query-rewrite-function expr))
;;            (maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit)))
;;       (with-current-buffer buf
;;         (mu4e-headers-mode)
;;         (unless ignore-history
;;           ;; save the old present query to the history list
;;           (when mu4e~headers-last-query
;;             (mu4e~headers-push-query mu4e~headers-last-query 'past)))
;;         (setq
;;          ;; don't set mode-name, s'il vous plaît
;;          ;; mode-name "mu4e-headers"
;;          mu4e~headers-last-query rewritten-expr)
;;         (add-to-list 'global-mode-string
;;                      '(:eval
;;                        (concat
;;                         (propertize
;;                          (mu4e~quote-for-modeline mu4e~headers-last-query)
;;                          'face 'mu4e-modeline-face)
;;                         " "
;;                         (mu4e-context-label)
;;                         (if (and mu4e-display-update-status-in-modeline
;;                                  (buffer-live-p mu4e~update-buffer)
;;                                  (process-live-p (get-buffer-process
;;                                                   mu4e~update-buffer)))
;;                             (propertize " (updating)" 'face 'mu4e-modeline-face)
;;                           "")))))

;;       ;; when the buffer is already visible, select it; otherwise,
;;       ;; switch to it.
;;       (unless (get-buffer-window buf 0)
;;         (switch-to-buffer buf))
;;       (run-hook-with-args 'mu4e-headers-search-hook expr)
;;       (mu4e~headers-clear mu4e~searching)
;;       (mu4e~proc-find
;;        rewritten-expr
;;        mu4e-headers-show-threads
;;        mu4e-headers-sort-field
;;        mu4e-headers-sort-direction
;;        maxnum
;;        mu4e-headers-skip-duplicates
;;        mu4e-headers-include-related)))

;;   )
