;;; ui/durand-modeline/config.el -*- lexical-binding: t; -*-

;;* buffer file name style

;;;###autoload
(setf doom-modeline-buffer-file-name-style 'buffer-name
      doom-modeline-persp-name t)

(after! doom-modeline
  (add-to-list 'doom-modeline-fn-alist (cons 'buffer-info-durand 'doom-modeline-segment--buffer-info-durand))

  (add-to-list 'doom-modeline-fn-alist
               (cons 'buffer-position-durand
                     'doom-modeline-segment--buffer-position-durand))
  (add-to-list 'doom-modeline-fn-alist
               (cons 'org-agenda
                     'doom-modeline-segment--org-agenda))

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
      org-agenda
      misc-info
      persp-name
      ;; battery
      ;; grip
      ;; irc
      mu4e
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
      checker)))

(after! doom-modeline
  (advice-add 'durand-narrow-dwim :after 'doom-modeline--buffer-narrow-icon-durand)
  (add-hook! 'doom-modeline-mode-hook :append
    (setq-default mode-line-format '("%e" (:eval (doom-modeline-format--durand))))))
(setf mode-line-format '("%e" (:eval (doom-modeline-format--durand))))

;;* doom-emacs automatically modifies the mode line format for pdf mode, so I
;; want to stop it.
(after! pdf-view
  (add-hook 'pdf-view-mode-hook #'set-nil-mode-line)
  (setf pdf-view-mode-hook (remq 'doom-modeline-set-pdf-modeline pdf-view-mode-hook)))


(add-hook 'doom-escape-hook 'durand-update-buffer-file-state-icon)
