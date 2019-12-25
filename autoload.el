;;; ~/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun durand-toggle-hl-todo ()
  "Toggle `hl-todo-mode'."
  (interactive)
  (cond
   (hl-todo-mode
    (hl-todo-mode -1))
   ((not hl-todo-mode)
    (hl-todo-mode 1))))

;; NOTE: when in terminal mode, don't display icons. And I need to check for GUI
;; in a hook
;;;###autoload
(cl-defun frame-init-behaviour (&optional (frame (selected-frame)))
  "Disable displaying icons in the mode line when run in a terminal"
  (with-selected-frame frame
    (cond
     ((display-graphic-p nil)
      (setf doom-modeline-icon t))
     (t
      (setf doom-modeline-icon nil)
      (set-frame-parameter nil 'menu-bar-lines 0)))))

;;* count the size of a buffer
;;;###autoload
(defun durand-file-size (&optional arg)
  "Show the buffer size in echo area.
If ARG is non-nil, show raw file size;
if ARG is nil, then show human-readable format."
  (interactive "P")
  (message
   "%s"
   (cond
    (arg
     (- (point-max) (point-min)))
    (t
     (file-size-human-readable
      (- (point-max) (point-min)))))))

;;;###autoload
(defun show-buffer-name (&optional arg)
  "Show the name of the buffer in echo area.
If ARG is non-nil, show the full name of the buffer."
  (interactive "P")
  (cond
   (arg
    (message (buffer-file-name)))
   (t
    (message (buffer-name)))))

;;* flyspell save a word to the current dictionary
;;;###autoload
(defun durand-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (cond
     ((consp word)
      (flyspell-do-correct
       'save
       nil
       (car word)
       current-location
       (cadr word)
       (caddr word)
       current-location))
     (t
      (message "No word to save.")))))

;;;###autoload
(defun compile-and-run-c (&optional arg)
  "Compile and run in c-mode"
  (interactive "P")
  (if (null arg)
      (make-process
       :name "*compilation*"
       :buffer "*C compilation*"
       :command '("make" "main")
       :sentinel (lambda (_process event-string)
                   (if (string-prefix-p "finished" event-string)
                       (let ((default-directory (file-name-directory (buffer-file-name))))
                         (make-process
                          :name "run"
                          :buffer "*running*"
                          :command (list (concat (file-name-as-directory default-directory) c-program-name))))
                     (user-error "There is a problem!")
                     (switch-to-buffer "C compilation"))))
    (make-process
     :name "*Instruments*"
     :buffer nil
     :command '("open" "/Applications/Xcode.app/Contents/Applications/Instruments.app"))))
