;;; emacs/durand-ibuffer/config.el -*- lexical-binding: t; -*-

(after! ibuffer
  (define-ibuffer-filter durand-bongo
      "Group bongo buffers together."
    (:description "Bongo buffers together"
     :reader (read-string "no effect: "))
    (with-current-buffer buf
      (cond
       ((derived-mode-p 'dired-mode)
        (let ((bongo-dirs durand-bongo-music-dir)
              found)
          (message "bd: %S" bongo-dirs)
          (while (and (not found)
                      (consp bongo-dirs))
            (cond
             ((file-in-directory-p default-directory (car bongo-dirs))
              (setq found t))
             (t (setq bongo-dirs (cdr bongo-dirs)))))
          found))
       ((derived-mode-p 'bongo-playlist-mode 'bongo-library-mode)))))
  (defun durand-bongo-set-filter ()
    "Set my custom filters."
    (interactive)
    (setq ibuffer-filter-groups
          (cons (cons "Bongo" '((durand-bongo)))
                ibuffer-filter-groups))
    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))
  (add-hook 'ibuffer-hook 'durand-bongo-set-filter 100))
