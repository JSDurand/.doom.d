;;; tools/sese/config.el -*- lexical-binding: t; -*-

(use-package! sese
  :load-path "~/.doom.d/modules/tools/sese/sese.el"
  :mode (("\\.sese\\'" . sese-mode))
  :config

  (map! :map sese-mode-map
        "C-j" 'sese-open-this-video
        "C-k" 'sese-video-replay
        "C-f" 'sese-video-fwd-1-sec
        "C-S-F" 'sese-video-fwd-5-sec
        "C-M-f" 'sese-video-fwd-10-sec
        "C-b" 'sese-video-rew-1-sec
        "C-S-b" 'sese-video-rew-5-sec
        "C-M-b" 'sese-video-rew-10-sec
        "M-j" 'sese-reset-start
        "M-k" 'sese-reset-end))
