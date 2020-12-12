;;; app/durand-bongo/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun contrib/bongo-add-dired-files ()
  "Add marked files inside of a Dired buffer to the Bongo library."
  (interactive)
  (let (file-point file files)
    (dired-map-over-marks
     (setq file-point (dired-move-to-filename)
           file (dired-get-filename)
           files (append files (list file)))
     nil t)
    (ignore file-point)
    (with-current-buffer (bongo-playlist-buffer)
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
the currently-playing track.

Modified by Durand so that this also runs while not in a bongo
buffer."
  (interactive)
  (with-bongo-playlist-buffer
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
    ;; (prot/bongo-play-random)
    ))

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
    (with-bongo-playlist-buffer
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
      ;; (prot/bongo-play-random)
      )))

;;;###autoload
(defun durand-bongo-dired-ivy-find-to-add ()
  "Use `durand-choose-list' to find files to add to bongo."
  (interactive)
  (require 'counsel)
  (require 'grep)
  (counsel-require-program find-program)
  ;; (cl-assert
  ;;  (and (derived-mode-p 'dired-mode)
  ;;       (file-in-directory-p default-directory bongo-default-directory)))
  (let* ((default-directory
           (cond
            ((and (derived-mode-p 'dired-mode)
                  (file-in-directory-p default-directory bongo-default-directory))
             default-directory)
            (t
             bongo-default-directory)))
         (all-files (counsel--find-return-list counsel-file-jump-args))
         (base default-directory)
         (chosen-files (durand-choose-list (mapcar 'list all-files) nil "Choose music to add: " t
                                           nil nil t nil t)))
    (cl-loop for song in chosen-files
             do
             (with-bongo-playlist-buffer
               (bongo-insert-file
                (file-truename (expand-file-name song base)))))
    ;; (ivy-read "Choose music to add: " all-files
    ;;           :require-match t
    ;;           :action (lambda (f)
    ;;                     (with-current-buffer bongo-default-playlist-buffer-name
    ;;                       (bongo-insert-file
    ;;                        (file-truename (expand-file-name f base))))))
    ))

;; REVIEW: This is oft not overriding the original function, to the effect that
;; the first time bongo starts, this function does not work, and I have to
;; define this function again, in order to play the music. So I decided to make
;; this into an overriding advice.
;;;###autoload
(defadvice! durand-bongo-compose-remote-option (socket-file)
  "Get the command line argument for starting mpv's remote interface at SOCKET-FILE.
This has to be fixed for mpv to work, since its argument parsing
convention is changed."
  :override 'bongo-compose-remote-option
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

;;;###autoload
(defun durand-bongo-play-first ()
  "Play the first song."
  (interactive)
  (with-bongo-playlist-buffer
    (with-imminent-bongo-player-start
      (bongo-stop)
      (when bongo-mark-played-tracks
        (bongo-mark-current-track-line-as-played))
      (goto-char (or (bongo-point-at-first-track-line)
                     (prog1 (point)
                       (message "No track in the playlist buffer."))))
      (bongo-set-current-track-position)
      (bongo-start))))

;;;###autoload
(defun durand-bongo-play-last ()
  "Play the first song."
  (interactive)
  (with-bongo-playlist-buffer
    (with-imminent-bongo-player-start
      (bongo-stop)
      (when bongo-mark-played-tracks
        (bongo-mark-current-track-line-as-played))
      (goto-char (or (bongo-point-at-last-track-line)
                     (prog1 (point)
                       (message "No track in the playlist buffer."))))
      (bongo-set-current-track-position)
      (bongo-start))))


;; NOTE: Fix a bug: there is no face called `modeline'; it should be
;; `mode-line'.
;;;###autoload
(defadvice! durand-bongo-seek ()
  "Interactively seek in the current Bongo track."
  :override 'bongo-seek
  (interactive)
  (setq bongo-seek-buffer (get-buffer-create "*Bongo Seek*"))
  (if bongo-seek-electric-mode
      (unwind-protect
          (save-window-excursion
            (require 'electric)
            (message nil)
            (let ((garbage-collection-messages nil)
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

;;; HACK: Redefine a function to go to the bongo-default-directory when no track
;;; is under point.
;;;
;;;###autoload
(defadvice! durand-bongo-dired-line (&optional point)
  "Open a Dired buffer containing the track at POINT.
Modified by Durand."
  :override 'bongo-dired-line
  (interactive)
  ;;; NOTE: The body-form inside condition-case is the original function body.
  (condition-case nil
      (save-excursion
        (bongo-goto-point point)
        (bongo-snap-to-object-line)
        (dired (file-name-directory
                (save-excursion
                  (while (bongo-header-line-p)
                    (bongo-down-section))
                  (if (bongo-local-file-track-line-p)
                      (bongo-line-file-name)
                    (error "No local file track here")))))
        (bongo-dired-library-mode 1))
    ('error (dired bongo-default-directory))))

;;; NOTE: I would like bongo to jump to bongo-playlist-buffer immediately,
;;; otherwise sometimes it jumps to the dired buffer, which is annoying.

;;;###autoload
(defadvice! durand-bongo-buffer ()
  "Return the buffer (bongo-playlist-buffer)."
  :override 'bongo-buffer
  (interactive)
  (bongo-playlist-buffer))

;;; hydra

;; NOTE: I would like to have a hydra to do some basic bongo commands, like go
;; to the next song, or control the volume.

;;;###autoload
(defun durand-bongo-next-song (&optional n)
  "Play the next N song."
  (interactive "p")
  (with-bongo-playlist-buffer
    (durand-bongo-play-next-or-first n)))

;;;###autoload
(defun durand-bongo-previous-song (&optional n)
  "Play the previous N song."
  (interactive "p")
  (with-bongo-playlist-buffer
    (durand-bongo-play-previous-or-last n)))

;;;###autoload
(defun durand-bongo-seek-anywhere ()
  "A wrapper around `durand-bongo-seek'."
  (interactive)
  (with-bongo-playlist-buffer
    (durand-bongo-seek)
    (setf durand-bongo-hydra-volume-return-p t)))

;;;###autoload
(defun bongo-seek-forward-5 (&optional n)
  "Seek 5 N seconds forward in the currently playing track."
  (interactive "p")
  (bongo-seek-forward (* 5 (or n 1))))

;;;###autoload
(defun bongo-seek-backward-5 (&optional n)
  "Seek 5 N seconds backward in the currently playing track."
  (interactive "p")
  (bongo-seek-backward (* 5 (or n 1))))

;;;###autoload
(defun durand-bongo-kill-line ()
  "Kill the currently playing bongo line."
  (interactive)
  (with-bongo-playlist-buffer
    (bongo-recenter)
    (bongo-kill-line)))

;; NOTE: I would like to stay in my hydra after setting the volume, so I wrote
;; this hacky workaround.

;;;###autoload
(defvar durand-bongo-hydra-volume-return-p nil
  "Whether to return to the hydra after quitting the volume buffer.")

;;;###autoload
(defun durand-bongo-volume ()
  "Stay in the hydra after adjusting the volume."
  (interactive)
  (volume)
  (setf durand-bongo-hydra-volume-return-p t))

;;;###autoload
(defadvice! durand-bongo-volume-call-hydra-a (&rest _args)
  "Call `durand-bongo-hydra/body' if `durand-bongo-hydra-volume-return-p' is non-nil."
  :after '(volume-quit bongo-seek-quit)
  (when durand-bongo-hydra-volume-return-p
    (durand-bongo-hydra/body)))

;;;###autoload
(defhydra durand-bongo-hydra (nil
                              nil
                              :hint nil
                              :color blue
                              :pre (setf durand-bongo-hydra-volume-return-p nil))
  "
_q_: quit    _n_: next song       _f_  : forward  5      _F_: forward  10  _<tab>_: show
_v_: volume  _p_: previous song   _b_  : backward 5      _B_: backward 10
_s_: seek    _d_: clear           _SPC_: pause / resume  _<_: first
_m_: bongo   _i_: insert          _I_  : playlist        _>_: last
"
  ("q" nil)
  ("v" durand-bongo-volume)
  ("s" durand-bongo-seek-anywhere)
  ("m" bongo)
  ("n" durand-bongo-next-song :color amaranth)
  ("p" durand-bongo-previous-song :color amaranth)
  ("d" prot/bongo-clear-playlist-and-stop :color amaranth)
  ("i" durand-bongo-dired-ivy-find-to-add :color amaranth)
  ("f" bongo-seek-forward-5 :color amaranth)
  ("b" bongo-seek-backward-5 :color amaranth)
  ("SPC" bongo-pause/resume :color amaranth)
  ("I" durand-bongo-insert-delete-playlist :color amaranth)
  ("F" bongo-seek-forward-10 :color amaranth)
  ("B" bongo-seek-backward-10 :color amaranth)
  ("<" durand-bongo-play-first :color amaranth)
  (">" durand-bongo-play-last :color amaranth)
  ("<tab>" bongo-show :color amaranth))

;;; NOTE: volume-set does not refresh the display, so let's fix that.

(defadvice! durand-bongo-refresh-after-set-a (&rest _args)
  "Refresh the display after we set the volume."
  :after 'volume-set
  (volume-redisplay))

;;; Don't insert text in the playlist buffer.

;;;###autoload
(defadvice! durand-bongo-default-playlist-buffer-a ()
  "Don't insert text in the playlist buffer."
  :override #'bongo-default-playlist-buffer
  (or (get-buffer bongo-default-playlist-buffer-name)
      (let ((buffer (get-buffer-create bongo-default-playlist-buffer-name)))
        (prog1 buffer
          (with-current-buffer buffer
            (bongo-playlist-mode))))))
