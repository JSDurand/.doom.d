;;; ui/durand-modeline/config.el -*- lexical-binding: t; -*-

(when (featurep! :ui modeline +light)
  (defvar durand-buffer-name-max 50)

  (setf +modeline-buffer-identification
        '((:eval
           (concat
            ;; (when (buffer-narrowed-p)
            ;;   (propertize
            ;;    (concat
            ;;     (+modeline-icon
            ;;      'material
            ;;      "vertical_align_center"
            ;;      "↕"
            ;;      "><")
            ;;     " ")
            ;;    'face 'warning
            ;;    'help-echo "Narrowed"))
            (propertize
             (s-truncate durand-buffer-name-max (buffer-name) "...")
             'face (cond ((buffer-modified-p)
                          '(error bold mode-line-buffer-id))
                         ((+modeline-active)
                          'mode-line-buffer-id))
             'help-echo buffer-file-name)))
          (buffer-read-only (:propertize " RO" face warning))))

  (def-modeline-var! +modeline-evil-state
    '(:eval
      (let ((str (+modeline-evil-state)))
        (unless (string-empty-p str)
          (concat str " ")))))

  (setf +modeline-position
        '(:eval (durand-modeline-position)))

  (setf +modeline-modes
        '(""
          (:propertize mode-name
           face bold
           mouse-face doom-modeline-highlight)
          mode-line-process
          " "))

  (def-modeline! :main
    '(""
      " "
      +modeline-evil-state
      +modeline-buffer-identification
      +modeline-position
      +modeline-matches)
    `(""
      mode-line-misc-info
      +modeline-modes
      (vc-mode ("  "
                ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
                vc-mode " "))
      "  "
      ;; +modeline-encoding
      "  "
      (+modeline-checker ("" +modeline-checker "   ")))))

(unless (featurep! :ui modeline +light)
  (after! doom-modeline
    (setf doom-modeline-buffer-file-name-style 'buffer-name
          doom-modeline-persp-name t)
    (add-to-list 'doom-modeline-fn-alist (cons 'buffer-info-durand 'doom-modeline-segment--buffer-info-durand))

    (add-to-list 'doom-modeline-fn-alist
                 (cons 'buffer-position-durand
                       'doom-modeline-segment--buffer-position-durand))
    (add-to-list 'doom-modeline-fn-alist
                 (cons 'org-agenda
                       'doom-modeline-segment--org-agenda))


    (advice-add #'doom-modeline--font-height :override #'durand-doom-modeline--font-height-a)

    (doom-modeline-def-modeline 'durand
      '(bar
        ;; workspace-name
        ;; window-number
        modals
        buffer-info-durand
        remote-host
        buffer-position-durand
        matches
        ;; parrot
        ;; selection-info
        )
      '(;; objed-state
        ;; org-agenda
        misc-info
        ;; persp-name
        ;; battery
        ;; grip
        ;; irc
        ;; mu4e
        ;; github
        debug
        lsp
        ;; minor-modes
        input-method
        indent-info
        ;; buffer-encoding
        major-mode
        ;; process
        vcs
        checker))))

(unless (featurep! :ui modeline +light)
  (after! doom-modeline
    (advice-add 'durand-narrow-dwim :after 'doom-modeline--buffer-narrow-icon-durand)
    (add-hook! 'doom-modeline-mode-hook :append
      (setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand)))))))
(setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))

;;* doom-emacs automatically modifies the mode line format for pdf mode, so I
;; want to stop it.
(after! pdf-view
  (add-hook 'pdf-view-mode-hook #'set-nil-mode-line)
  (setf pdf-view-mode-hook (remq 'doom-modeline-set-pdf-modeline pdf-view-mode-hook)))


(unless (featurep! :ui modeline +light)
  (add-hook 'doom-escape-hook 'durand-update-buffer-file-state-icon))

;; add task information to project modeline
;;
;; NOTE: For now this seems to be unnecessary information, so I am not using it
;; for now.

;; (doom-modeline-def-modeline 'project
;;   '(bar window-number buffer-default-directory)
;;   '(;; org-agenda
;;     misc-info mu4e debug major-mode process))

;; add timer to update modeline indicator every day

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
;;            #'durand-mu4e-open-if-necessary))))
