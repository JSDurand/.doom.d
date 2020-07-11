;;; app/durand-bongo/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun contrib/bongo-add-dired-files ()
  "Add marked files inside of a Dired buffer to the Bongo library."
  (interactive)
  (let (file-point file (files nil))
    (dired-map-over-marks
     (setq file-point (dired-move-to-filename)
           file (dired-get-filename)
           files (append files (list file)))
     nil t)
    (ignore file-point)
    (with-current-buffer bongo-default-playlist-buffer-name
      (mapc 'bongo-insert-file files))))

;;;###autoload
(defvar durand-bongo-music-dir nil
  "Directories to store my songs.
This is used since my music directories contain symbolic links.")

;;;###autoload
(defun durand-bongo-dired-library ()
  "Set `bongo-dired-library-mode' when accessing bongo-default-directory.
This is meant to be hooked to `dired-mode'.

Upon activation, the directory and all its sub-directories become
a valid library buffer for Bongo, from where we can, among
others, add tracks to playlists.

The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Adapted from Protesilaos' dotemacs."
  (when (cl-loop for dir in durand-bongo-music-dir
                 when (file-in-directory-p default-directory dir)
                 return t)
    (set (make-local-variable 'bongo-dired-library-mode) 't)
    (evil-emacs-state)))

;;;###autoload
(defun prot/bongo-clear-playlist-and-stop ()
  "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently-playing track."
  (interactive)
  (when (bongo-playlist-buffer-p)
    (bongo-stop)
    (bongo-erase-buffer)))

;;;###autoload
(defun prot/bongo-play-random ()
  "Play a random track with `bongo' and set random playback."
  (interactive)
  (when (or (bongo-playlist-buffer-p)
            (bongo-library-buffer-p))
    (bongo-play-random)
    (setf bongo-next-action 'durand-bongo-play-next-or-first)
    ;; (bongo-random-playback-mode 1)
    ))

;;;###autoload
(defun prot/bongo-library-insert-and-play-random ()
  "Add directory tree or marked items to the `bongo' playlist.
Create the playlist buffer if necessary.
This is meant to work while inside a `dired' buffer that doubles
as a library buffer (see `prot/bongo-dired-library')."
  (interactive)
  (when (bongo-library-buffer-p)
    (unless (bongo-playlist-buffer-p)
      (bongo-playlist-buffer))
    (contrib/bongo-add-dired-files)
    (prot/bongo-play-random)))

;;;###autoload
(defun durand-bongo-insert-delete-playlist ()
  "Insert or delete a `bongo' playlist.
The files are stored in a predetermined path inside the Music
directory. Upon insertion, playback starts immediately, in
accordance with `prot/bongo-play-random'.

Adapted from Protesilaos' dotemacs by Durand."
  (interactive)
  (let* ((path (file-name-as-directory
                (expand-file-name
                 "playlists"
                 bongo-default-directory)))
         (dotless directory-files-no-dot-files-regexp)
         (playlist (directory-files path t dotless)))
    (when (bongo-playlist-buffer-p)
      ;; (bongo-insert-playlist-contents
      ;;  (completing-read "Select playlist: " playlists nil t))
      (ivy-read "Choose a playlist file: " playlist
                :require-match t
                :caller 'durand-bongo-insert-delete-playlist
                :action '(1
                          ("i" bongo-insert-playlist-contents "insert playlist")
                          ("d" (lambda (file)
                                 (delete-file file)
                                 (setf (ivy-state-collection ivy-last)
                                       (directory-files
                                        (file-name-as-directory
                                         (expand-file-name
                                          "playlists"
                                          bongo-default-directory))
                                        t directory-files-no-dot-files-regexp))
                                 (ivy--reset-state ivy-last))
                           "delete playlist")
                          ("e" find-file "edit")))
      (prot/bongo-play-random))))

;;;###autoload
(defun durand-bongo-dired-ivy-find-to-add ()
  "Use `ivy' to find files to add to bongo."
  (interactive)
  (require 'counsel)
  (require 'grep)
  (counsel-require-program find-program)
  (cl-assert
   (and (derived-mode-p 'dired-mode)
        (file-in-directory-p default-directory bongo-default-directory)))
  (let* ((all-files (counsel--find-return-list counsel-file-jump-args))
         (base default-directory))
    (ivy-read "Choose music to add: " all-files
              :require-match t
              :action (lambda (f)
                        (with-current-buffer bongo-default-playlist-buffer-name
                          (bongo-insert-file
                           (file-truename (expand-file-name f base))))))))

;;;###autoload
(defun bongo-compose-remote-option (socket-file)
  "Get the command line argument for starting mpv's remote interface at SOCKET-FILE.
This has to be fixed for mpv to work, since its argument parsing
convention is changed."
  (when (equal bongo-mpv-remote-option 'unknown)
    (setq bongo-mpv-remote-option (bongo--mpv-get-remote-option)))
  (list (concat bongo-mpv-remote-option "=" socket-file)))

;;;###autoload
(defvar durand-bongo-save-playlist-hist nil
  "A variable that holds the history values for saving playlist names.")

;;;###autoload
(defun durand-bongo-save-playlist ()
  "Save the current playlist into a file."
  (interactive)
  (let ((playlist-name
         (expand-file-name
          (read-string "Playlist name:" nil 'durand-bongo-save-playlist-hist)
          (expand-file-name "playlists"
                            bongo-default-directory)))
        (append-p (y-or-n-p "Append? "))
        files end)
    (with-current-buffer bongo-default-playlist-buffer-name
      (save-excursion
        (goto-char (or (bongo-point-at-first-track-line)
                       (user-error
                        "No tracks in the playlist!")))
        (push (bongo-line-file-name) files)
        (while (not end)
          (if-let ((next-pos (bongo-point-at-next-track-line)))
              (progn
                (goto-char next-pos)
                (push (bongo-line-file-name) files))
            (setf end t)))))
    (with-temp-buffer
      (cl-loop for file in files
               do (progn
                    (insert file)
                    (newline)))
      (write-region nil nil playlist-name append-p))))

;;;###autoload
(defun durand-bongo-next-or-first (&optional n)
  "Make the next track current in the nearest playlist buffer.
If there is no next track, go to the first track.
With prefix argument N, skip that many tracks."
  (interactive "P")
  (if (bongo-playing-p)
      (durand-bongo-play-next-or-first n)
    (with-bongo-playlist-buffer
      (let ((line-move-ignore-invisible nil))
        (save-excursion
          (goto-char (or (bongo-point-at-current-track-line)
                         (bongo-point-at-first-track-line)
                         (user-error "No tracks in playlist")))
          (dotimes (_dummy (prefix-numeric-value n))
            (goto-char (or (bongo-point-at-next-track-line)
                           (bongo-point-at-first-track-line))))
          (bongo-set-current-track-position))))))

;;;###autoload
(defun durand-bongo-play-next-or-first (&optional n)
  "Start playing the next track in the nearest Bongo playlist buffer.
If there is no next track to play, signal an error.
With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
progressive playback mode.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
  (interactive "P")
  (cond
   ((equal n '(16))
    (bongo-insert-line 'bongo-action '(bongo-progressive-playback-mode)))
   ((equal n '(4))
    (bongo-progressive-playback-mode))
   ((consp n)
    (user-error "This prefix argument %S is not supported."
                n))
   ((< (prefix-numeric-value n) 0)
    (durand-bongo-play-previous-or-last (- (prefix-numeric-value n))))
   (t
    (with-imminent-bongo-player-start
      (bongo-stop)
      (when bongo-mark-played-tracks
        (bongo-mark-current-track-line-as-played))
      (durand-bongo-next-or-first n)
      (bongo-start)))))

;;;###autoload
(defun durand-bongo-previous-or-last (&optional n)
  "Make the previous track current in the nearest playlist buffer.
If there is no previous track, go to the last track.
With prefix argument N, skip that many tracks."
  (interactive "p")
  (if (bongo-playing-p)
      (durand-bongo-play-previous-or-last n)
    (with-bongo-playlist-buffer
      (let ((line-move-ignore-invisible nil))
        (save-excursion
          (goto-char (or (bongo-point-at-current-track-line)
                         (bongo-point-at-last-track-line)
                         (user-error "No tracks in playlist")))
          (dotimes (_dummy (prefix-numeric-value n))
            (goto-char (or (bongo-point-at-previous-track-line)
                           (bongo-point-at-last-track-line))))
          (bongo-set-current-track-position))))))

;;;###autoload
(defun durand-bongo-play-previous-or-last (&optional n)
  "Start playing the previous track in the nearest playlist buffer.
If there is no previous track to play, play the last track.
With numerical prefix argument N, skip that many tracks.
With \\[universal-argument] as prefix argument, just switch to \
regressive playback mode.
With \\[universal-argument] \\[universal-argument] as prefix argument, \
insert an action track at point."
  (interactive "P")
  (cond
   ((equal n '(16))
    (bongo-insert-line 'bongo-action '(bongo-regressive-playback-mode)))
   ((equal n '(4))
    (bongo-regressive-playback-mode))
   ((consp n)
    (user-error "This prefix argument %S is not supported."
                n))
   ((< (prefix-numeric-value n) 0)
    (durand-bongo-play-next-or-first (- (prefix-numeric-value n))))
   (t
    (with-imminent-bongo-player-start
      (bongo-stop)
      (when bongo-mark-played-tracks
        (bongo-mark-current-track-line-as-played))
      (durand-bongo-previous-or-last n)
      (bongo-start)))))

;; NOTE: Fix a bug: there is no face called `modeline'; it should be
;; `mode-line'.
;;;###autoload
(defun bongo-seek ()
  "Interactively seek in the current Bongo track."
  (interactive)
  (setq bongo-seek-buffer (get-buffer-create "*Bongo Seek*"))
  (if bongo-seek-electric-mode
      (unwind-protect
          (save-window-excursion
            (require 'electric)
            (message nil)
            (let ((echo-keystrokes 0)
                  (garbage-collection-messages nil)
                  (bongo-seeking-electrically t))
              (ignore bongo-seeking-electrically)
              (set-window-buffer (minibuffer-window) bongo-seek-buffer)
              (select-window (minibuffer-window))
              (let ((old-local-map (current-local-map))
                    (old-global-map (current-global-map)))
                (use-local-map nil)
                (use-global-map bongo-seek-mode-map)
                (setq major-mode 'bongo-seek-mode)
                (unwind-protect
                    (progn
                      (bongo-seek-redisplay)
                      (run-hooks 'bongo-seek-mode-hook)
                      (catch 'bongo-seek-done
                        (Electric-command-loop
                         'bongo-seek-done
                         ;; Avoid `noprompt' due to
                         ;; a bug in electric.el.
                         '(lambda () 'noprompt)
                         nil
                         (lambda (_x _y) (bongo-seek-redisplay)))))
                  (use-local-map old-local-map)
                  (use-global-map old-global-map)))))
        (when bongo-seek-buffer
          (kill-buffer bongo-seek-buffer)
          (setq bongo-seek-buffer nil)))
    (cond
     ((null (get-buffer-window bongo-seek-buffer))
      (let ((window-min-height 2)
            (split-window-keep-point nil))
        (select-window
         (split-window-vertically
          (if (and (fboundp 'face-attr-construct)
                   ;; NOTE: bug occurs here.
                   (plist-get (face-attr-construct 'mode-line) :box))
              -3 -2)))
        (switch-to-buffer bongo-seek-buffer)))
     ((not (eq (current-buffer) bongo-seek-buffer))
      (select-window (get-buffer-window bongo-seek-buffer))))
    (bongo-seek-mode)
    ;; NOTE: added by Durand to start always in evil-emacs-state
    (evil-emacs-state)
    (setq buffer-read-only t)
    (bongo-seek-redisplay)))
