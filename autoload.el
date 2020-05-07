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
If in pdf-view-mode, show current page/total pages.
if ARG is nil, then show human-readable format."
  (interactive "P")
  (message
   "%s"
   (cond
    (arg
     (concat
      (number-to-string
       (- (point-max) (point-min)))
      ", i.e. "
      (file-size-human-readable
       (- (point-max) (point-min)))))
    ((derived-mode-p 'pdf-view-mode)
     (format "P%d/%d"
             (pdf-view-current-page)
             (pdf-info-number-of-pages)))
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

;;; kill karabiner

;;;###autoload
(defun kill-karabiner ()
  "Kill karabiner elements so that it can function again."
  (interactive)
  (shell-command
   (concat
    "echo "
    (shell-quote-argument (read-passwd "Password? "))
    " | sudo -S killall karabiner_grabber karabiner_observer")))

;;; get advice of a function

;;;###autoload
(defun durand-get-advices (fun)
  "Return the advices of FUN."
  (let (res)
    (advice-mapc
     (lambda (x y)
       (push (list x y) res))
     fun)
    res))

;;; meditating timer

;;;###autoload
(defvar durand-meditation-timer nil
  "Timer for meditating")

;;;###autoload
(defun durand-start-meditating (&optional custom-time)
  "Start counting for meditating.
When it times out, it will execute the function `durand-come-back'.
If CUSTOM-TIME is non-nil, then it will ask for the number of minutes to count.
The default is 25 minutes."
  (interactive "P")
  (run-with-timer
   (*
    (cond
     ((null custom-time) 25)
     (t (read-number "Le nombre de minutes de méditation: " 25)))
    60)
   nil
   'durand-come-back)
  (list-timers))

;;;###autoload
;; (defun sim-2-tw-chinese (str)
;;   (require 'opencc)
;;   (opencc-string "s2tw" str))

;; in ediff merger two variants without markers

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "B" 'ediff-copy-both-to-C))

;; org-noter fix

;; For a specific pdf file, org-noter seems to have some problems managing
;; properties, so an advice is needed.

;; NOTE: This not needed anymore, since I found out the problem: it is with the
;; function `org-entry-put'.
;;
;; (defadvice! durand-org-noter-kill-pro-noter-a (&rest _args)
;;   "Kill some pro notes section."
;;   :after 'org-noter
;;   (run-at-time
;;    "1" nil
;;    (lambda ()
;;      (org-noter--with-selected-notes-window
;;       nil (let ((inhibit-read-only t))
;;             (save-excursion
;;               (goto-char (point-min))
;;               (while (search-forward ":END:\n:PRO:NOTER_PAGE: 1PERTIES:\n\n" nil t)
;;                 (replace-match ""))))))))

;; deft parse title

(defun deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title.

This is modified by Durand so that for org files the title needs not be on the first line,
and for tex files the title is defined by the title macro."
  (if deft-use-filename-as-title
      (deft-base-filename file)
    (let ((begin (string-match "^.+$" contents)))
      (when begin
        (cond
         ((string-match-p "org$" file)
          (if (string-match "#\\+TITLE: \\([^\n]*\\)\n" contents)
              (match-string 1 contents)
            (deft-base-filename file)))
         ((string-match-p "tex$" file)
          (if (string-match "\\(?:[^% ]\\)\\\\tit\\(?:le\\)?{\\([^}]*\\)}" contents)
              (match-string 1 contents)
            (deft-base-filename file)))
         (t
          (funcall deft-parse-title-function
                   (substring contents begin (match-end 0)))))))))
