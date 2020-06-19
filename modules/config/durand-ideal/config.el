;;; config/durand-ideal/config.el -*- lexical-binding: t; -*-

(map! :map doom-leader-toggle-map [?t] #'durand-show-current-time)

;; add a custom keymap
(define-prefix-command 'durand-switch-buffer-map)
(define-key durand-switch-buffer-map [?\C-c ?q] 'minibuffer-keyboard-quit)
(define-key durand-switch-buffer-map [?\C-c ?h] 'durand-toggle-headlong)
(define-key durand-switch-buffer-map [?\C-c ?s]
  (lambda ()
    (interactive)
    (message "%s" (if durand-headlong
                      "head long activated"
                    "head long desactivated"))))

;; workspaced version is in `SPC b b'.
;; (map! :map doom-leader-buffer-map "b" #'durand-switch-buffer)

(use-package! persp-mode
  :after-call durand-switch-buffer
  :config

  (map! :leader
        "," 'durand-switch-buffer
        :n [?b ?v] 'ibuffer
        :n [?b ?b] 'durand-buffers-major-mode))

(map! :leader "wa" #'ace-window)

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

(set-evil-initial-state!
  '(durand-pdf-mode)
  'emacs)

(after! ivy-rich (setf ivy-rich-path-style 'absolute))
