;;; ui/durand-dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar durand-terminal-open-p nil
  "A variable to indicate whether or not the terminal is opened.")

;;;###autoload
(defun durand-terminal-filter (proc out)
  "Filter function to open or activate terminal.
This should only be used by `durand-open-terminal'.
PROC is the process, and OUT is the output."
  (cond
   ((string= (process-name proc) "terminal-detector"))
   (t (user-error "The filter is used in a wrong place, by %S" proc)))
  (setq durand-terminal-open-p (string= out "true")))

;;;###autoload
(defun durand-open-terminal (&optional arg)
  "Open terminal at the current directory."
  (interactive "P")
  (cond
   ((null arg)
    (+vterm/here nil))
   ((equal arg '(4))
    (make-process
     :name "terminal-detector"
     :command `("osascript" "-e"
                ,(format "tell application \"System Events\" to (name of processes) contains \"%s\""
                         "Terminal"))
     :filter #'durand-terminal-filter
     :sentinel #'ignore
     :buffer nil)
    (accept-process-output (get-process "terminal-detector"))
    (cond
     (durand-terminal-open-p
      (make-process
       :name "open-terminal"
       :command '("open" "-a" "terminal" "./")
       :buffer nil))
     (t
      (make-process
       :name "activate-terminal"
       :command '("osascript" "-e" "tell application \"Terminal\" to activate")
       :buffer nil))))
   ((equal arg '(16))
    (make-process
     :name "close-terminal"
     :buffer nil
     :command '("osascript" "-e" "tell application \"Terminal\" to quit")))))

;;;###autoload
(defun durand-open-dashboard ()
  "Open the dash board buffer.
Also reset the `mode-line-format' for convenience."
  (interactive)
  (switch-to-buffer (doom-fallback-buffer))
  (durand-refresh-dashboard-modeline))

;;;###autoload
(defun durand-open-discord (&optional arg)
  "Open Discord.
With ARG \\[universal-argument], close discord."
  (interactive "P")
  (let ((browsing-command (cond ((equal arg '(4))
                                 '("osascript" "-e" "tell application \"Discord\" to quit"))
                                (t
                                 '("open" "-a" "Discord")))))
    (make-process
     :name "Discord"
     :command browsing-command
     :buffer nil))
  (when (equal arg '(4))
    (message "Discord closed.")))

;;;###autoload
(defun durand-refresh-dashboard-modeline ()
  "Set the mode-line-format to the project mode-line.
Sometimes the mode-line-format of the fallback buffer is
accidentally set to `nil', so I want a function to reset it
automatically."
  (interactive)
  (with-current-buffer (doom-fallback-buffer)
    (setf mode-line-format '("%e" (:eval (doom-modeline-format--project))))))

;; NOTE: I think I should have a new module for this functionality.
;; ;;;###autoload
;; (defun durand-dashboard-widget-banner ()
;;   "Besides the original banner, also add some points recording the current efforts."
;;   (let ((point (point)))
;;     (mapc (lambda (line)
;;             (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
;;                                 'face 'doom-dashboard-banner) " ")
;;             (insert "\n"))
;;           '("=================     ===============     ===============   ========  ========"
;;             "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
;;             "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
;;             "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
;;             "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
;;             "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
;;             "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
;;             "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
;;             "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
;;             "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
;;             "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
;;             "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
;;             "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||"
;;             "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||"
;;             "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||"
;;             "||.=='    _-'                                                     `' |  /==.||"
;;             "=='    _-'                         E M A C S                          \\/   `=="
;;             "\\   _-'                                                                `-_   /"
;;             " `''                                                                      ``'"))
;;     (when (and (display-graphic-p)
;;                (stringp fancy-splash-image)
;;                (file-readable-p fancy-splash-image))
;;       (let ((image (create-image (fancy-splash-image-file))))
;;         (add-text-properties
;;          point (point) `(display ,image rear-nonsticky (display)))
;;         (save-excursion
;;           (goto-char point)
;;           (insert (make-string
;;                    (truncate
;;                     (max 0 (+ 1 (/ (- +doom-dashboard--width
;;                                       (car (image-size image nil)))
;;                                    2))))
;;                    ? ))))
;;       ;; (insert "bonjour!")
;;       (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
;;                            ?\n)))))
