;;; app/durand-bongo/config.el -*- lexical-binding: t; -*-

(use-package! bongo
  :commands bongo
  ;; :init
  ;; (require 'volume "/Users/durand/.doom.d/modules/app/durand-bongo/volume.el")
  :config
  ;; NOTE: I don't like electic mode.
  (setf volume-electric-mode nil)

  (set-evil-initial-state! 'volume-mode 'emacs)

  (setf bongo-default-directory (expand-file-name "~/Desktop/Centre/Musique"))
  (setf bongo-prefer-library-buffers nil)
  (setf bongo-insert-whole-directory-trees t)
  (setf bongo-logo nil)
  (setf bongo-action-track-icon nil)
  (setf bongo-display-track-icons nil)
  (setf bongo-display-track-lengths nil)
  (setf bongo-display-header-icons nil)
  (setf bongo-display-playback-mode-indicator t)
  (setf bongo-display-inline-playback-progress nil)
  (setf bongo-mark-played-tracks nil)
  (setf bongo-header-line-mode nil)
  (setf bongo-header-line-function nil)
  (setf bongo-mode-line-indicator-mode nil)
  (setf bongo-enabled-backends '(mpv))
  (setf bongo-seek-electric-mode nil)
  (add-hook 'bongo-playlist-mode-hook 'evil-emacs-state)
  (setf bongo-custom-backend-matchers
        '((mpv local-file "webm" "m4a")))
  (setq-default bongo-next-action 'durand-bongo-play-next-or-first)
  (setf durand-bongo-music-dir
        (cons bongo-default-directory
              (cl-loop for file in (directory-files-recursively
                                    bongo-default-directory
                                    ".*" t nil t)
                       for attr = (car (file-attributes file))
                       if (and (stringp attr)
                               (file-directory-p attr))
                       collect attr)))
  (map! :map durand-view-map
        [?m] 'bongo
        :map bongo-dired-library-mode-map
        [?\C-j] 'durand-bongo-dired-ivy-find-to-add
        [?\C-c ?n] 'durand-bongo-play-next-or-first
        [?\C-c ?p] 'durand-bongo-play-previous-or-last
        :map bongo-playlist-mode-map
        [?j] 'durand-bongo-save-playlist
        [?\C-c ?n] 'durand-bongo-play-next-or-first
        [?\C-c ?p] 'durand-bongo-play-previous-or-last)

  ;; seek mode map additions

  (map! :map bongo-seek-mode-map
        [?t] 'bongo-seek-to)

  :hook ((dired-mode . durand-bongo-dired-library)
         (bongo-playlist-mode . hl-line-mode))
  :bind (:map bongo-playlist-mode-map
         ("C-d" . prot/bongo-clear-playlist-and-stop)
         ("I" . durand-bongo-insert-delete-playlist)
         :map durand-view-map
         ("m" . bongo)
         :map bongo-dired-library-mode-map
         ("<C-return>" . prot/bongo-library-insert-and-play-random)))

(use-package! volume
  :commands volume
  :load-path "/Users/durand/.doom.d/modules/app/durand-bongo/"
  :config
  (add-hook 'volume-mode-hook 'evil-emacs-state))
