;;; .doom.d/modules/lang/durand-tex/autoload.el -*- lexical-binding: t; -*-

(require 'tex)
(require 'ivy)
(require 'latex)
(require 'pdf-view)

;; (define-derived-mode durand-tex-output-mode special-mode "DTOutput"
;;   "Keymap used in the output buffer of the tex command.")

;; (define-key durand-tex-output-mode-map [?q] 'quit-window)
;; (define-key durand-tex-output-mode-map [?q] )

;;;###autoload
(defun tex ()
  "Command to compile TeX
If TEX-ROOT-FILE-NAME is non-nil, use it as the file name to compile,
else use the current file name.
I found using call-process better, as it is more minimal."
  (interactive)
  (unless (derived-mode-p 'tex-mode)
    (user-error "Not in a tex buffer"))
  (let* ((fnb (or tex-root-file-name
                  (file-name-base)))
         (bfn (buffer-file-name))
         (durand-tex-command (or (and (string= major-mode "plain-tex-mode") "xetex")
                                 "xelatex"))
         (output-bn "*durand-tex*")
         (default-directory (or (and bfn (file-name-directory bfn))
                                (error "Ce tampon n'est pas associé à un fichier"))))
    (when (get-buffer output-bn)
      (kill-buffer output-bn))
    (call-process "texfot" nil output-bn t durand-tex-command fnb)
    (setf tex-output-bn output-bn)
    (display-buffer (get-buffer output-bn)))
  (special-mode)
  (evil-emacs-state)
  ;; (map! :map TeX-mode-map [f9] #'tex-first-pdf)
  ;; (setf durand-tex-action 'tex-first-pdf
  ;;       tex-changed t)
  )

(set-popup-rule! "\*durand-tex\*"
  :side 'bottom
  :size 0.2
  :modeline nil
  :quit t
  :select t
  :ttl 5)

;;;###autoload
(defun tex-display-or-kill-temp-buffer (&optional arg bn)
  "display and fit to size the buffer BN
If ARG is non-nil, delete the buffer BN"
  (interactive (list current-prefix-arg nil))
  (let ((bn (or bn tex-output-bn)))
    (cond
     (arg
      (when (get-buffer bn)
        (delete-windows-on bn)
        (kill-buffer bn)))
     ((and bn
           (get-buffer bn)
           (buffer-live-p (get-buffer bn))
           (get-buffer-window bn))
      (delete-windows-on bn))
     ((and bn (get-buffer bn) (buffer-live-p (get-buffer bn)))
      (with-current-buffer bn
        (goto-char (point-min)))
      (display-buffer bn)
      ;; (resize-temp-buffer-window (get-buffer-window bn))
      )
     ((null bn)
      (user-error "`tex-output-bn' is nil."))
     (t
      (user-error "No buffer named %s." bn)))))
;;;###autoload
(defun tex-action-pdf ()
  "Action invoked in pdf buffer in a tex working cycle"
  (interactive)
  (switch-to-buffer (or working-name
                        (concat (file-name-sans-extension (buffer-name)) ".tex")))
  (advice-add 'save-buffer :before 'save-tex-advice)
  (map! :map TeX-mode-map [f9] 'tex-tex-go-to-pdf
        :map LaTeX-mode-map [f9] 'tex-tex-go-to-pdf)
  (setf durand-tex-action 'tex-pdf-go-to-tex-or-vice-versa))

;;;###autoload
(defun save-tex-advice (&rest _arg)
  (interactive)
  (cond ((string-equal major-mode "plain-tex-mode")
         (map! :map TeX-mode-map
               [f9] #'tex)
         (advice-remove 'save-buffer 'save-tex-advice))
        ((string-equal major-mode "latex-mode")
         (map! :map LaTeX-mode-map
               [f9] #'tex)
         (advice-remove 'save-buffer 'save-tex-advice))))

;;;###autoload
(defun tex-action-eshell ()
  "The function invoked when in eshell during a tex-working cycle"
  (interactive)
  (let* ((full-name (buffer-file-name (other-buffer)))
         (full-pdf-name (concat (or tex-root-file-name
                                    (file-name-sans-extension full-name))
                                ".pdf"))
         (pdf-name (or (and tex-root-file-name
                            (concat tex-root-file-name ".pdf"))
                       (file-name-nondirectory full-pdf-name))))
    (if (get-buffer pdf-name)
        (switch-to-buffer pdf-name)
      (find-file full-pdf-name))
    (tex-pdf-prepare)))

;;;###autoload
(defun tex-pdf-prepare ()
  "Preparative work needed in pdf buffer"
  (revert-buffer)
  (delete-other-windows)
  (map! :map pdf-view-mode-map
        [f9] #'tex-action-pdf)
  (setf durand-tex-action 'tex-action-pdf))
;;;###autoload
(defun tex-tex-go-to-pdf ()
  "Go to the corresponding pdf file to the current tex file"
  (interactive)
  (let ((corresponding-pdf (concat (or tex-root-file-name
                                       (file-name-sans-extension (buffer-name)))
                                   ".pdf")))
    (cond
     ((and corresponding-pdf (get-buffer corresponding-pdf))
      (switch-to-buffer corresponding-pdf))
     ((file-exists-p corresponding-pdf)
      (find-file corresponding-pdf))
     (t
      (user-error "Cannot find pdf named %s" corresponding-pdf)))
    (map! :map pdf-view-mode-map [f9] #'tex-pdf-go-to-tex)
    (setf durand-tex-action 'tex-pdf-go-to-tex-or-vice-versa)))

;;;###autoload
(defun tex-first-pdf ()
  "First time switching to pdf"
  (interactive)
  (let ((corresponding-pdf (concat (or tex-root-file-name
                                       (file-name-sans-extension (buffer-name)))
                                   ".pdf")))
    (cond
     ((get-buffer corresponding-pdf)
      (switch-to-buffer corresponding-pdf)
      (tex-pdf-prepare))
     ((file-exists-p corresponding-pdf)
      (find-file corresponding-pdf)
      (tex-pdf-prepare))
     (t
      (message "Cannot find pdf named %s" corresponding-pdf)))))

;;;###autoload
(defun tex-pdf-go-to-tex ()
  "Go to the corresponding tex file to the current pdf file"
  (interactive)
  (switch-to-buffer (or working-name
                        (concat (file-name-sans-extension (buffer-name)) ".tex"))))

;;;###autoload
(defun tex-set-up-root (arg)
  "Set up TEX-ROOT-FILE-NAME using (file-name-base)"
  (interactive "P")
  (if (null arg)
      (progn (setq tex-root-file-name (file-name-base))
             (message (format "tex-root-file-name set to %s" tex-root-file-name)))
    (progn (setq tex-root-file-name nil)
           (message (format "tex-root-file-name set to %s" tex-root-file-name)))))

;;;###autoload
(defun tex-set-up-working (arg)
  "Set up WORKING-NAME using (buffer-name)"
  (interactive "P")
  (if (null arg)
      (progn (setq working-name (buffer-name))
             (message (format "working-name set to %s" working-name)))
    (progn (setq working-name nil)
           (message (format "working-name set to %s" working-name)))))

;;;autoload
(defun tex-pdf-go-to-tex-or-vice-versa ()
  "Either go to pdf or go to tex"
  (interactive)
  (cond
   (current-prefix-arg
    (tex-display-or-kill-temp-buffer))
   ((derived-mode-p 'pdf-view-mode)
    (tex-pdf-go-to-tex))
   ((derived-mode-p 'tex-mode)
    (tex-tex-go-to-pdf))
   (t
    (user-error "Not in a pdf file or tex file."))))

;; below do not depend on packages

;;;###autoload
(defun durand-delete-pair ()
  "Delete the matching pair"
  (interactive)
  (cond (view-mode ; if in view-mode, then scroll down
         (View-scroll-page-backward))
        ((region-active-p) ; if the region is active, then do the original thing
         (delete-char 1))
        ((memq (char-before) '(?\( ?\[ ?\{))
         (save-excursion
           (backward-char 1)
           (ignore-errors
             (forward-sexp 1)
             (delete-char -1)))
         (delete-char -1))
        (t
         (delete-char -1))))

;;;###autoload
(defun end-exit-paren ()
  "Use closing pasenthesis to exit the parenthesis"
  (interactive)
  (let ((ch (char-after nil))
        (ch-list '(?\) ?\} ?\] ?\$)))
    (cond ((memq ch ch-list) (forward-char))
          (t (insert ")")))))

;;;###autoload
(defun open-back-paren ()
  "Use closing pasenthesis to exit the parenthesis"
  (interactive)
  (let ((ch (char-before nil))
        (ch-list '(?\) ?\} ?\] ?\$)))
    (cond ((memq ch ch-list) (backward-char))
          (t (insert "ç")))))

;;;###autoload
(defun open-paren ()
  "open parenthesis inserts a matching pair"
  (interactive)
  (progn
    (insert "()")
    (backward-char)))

;;;###autoload
(defun open-curly ()
  "open curly inserts a matching pair"
  (interactive)
  (progn
    (insert "{}")
    (backward-char)))

;;;###autoload
(defun open-bracket ()
  "open bracket inserts a matching pair"
  (interactive)
  (progn
    (insert "[]")
    (backward-char)))

;;;###autoload
(defun insert-def ()
  "my function to insert defs of tex documents easily"
  (interactive)
  (let ((name (read-string "Enter macro name: "))
        (body (buffer-substring-no-properties (mark) (point))))
    (if (use-region-p)
        (progn (kill-region (region-beginning) (region-end))
               (insert (format "\\%s" name))
               (save-excursion
                 (goto-char (point-min))
                 (setq temp (search-forward-regexp "^\\\\def\\Sw" nil t))
                 (when temp
                   (message "Macro inserted.")
                   (beginning-of-line)
                   (while (re-search-forward "^\\\\def\\Sw" nil t)
                     (re-search-forward "{" nil t)
                     (backward-char 1)
                     (forward-sexp))
                   (open-line 1)
                   (forward-char 1)
                   (insert (format "\\def\\%s{%s}" name body))))
               (if (not temp)
                   (save-excursion (message "No defs found, insert in the above paragragh.")
                                   (backward-paragraph)
                                   (insert (format "\n\\def\\%s{%s}" name body)))))
      (message "Please activate region which contains the definiton before inserting the def"))))

;;;###autoload
(defun one-def ()
  "insert defonetext instead of def"
  (interactive)
  (let ((name (read-string "Enter macro name: ")))
    (progn (insert (format "\\%s" (downcase name)))
           (save-excursion
             (goto-char (point-min))
             (setq temp (search-forward-regexp "^\\\\def" nil t))
             (when temp
                   (message "Macro inserted.")
                   (beginning-of-line)
                   (while (re-search-forward "^\\\\def" nil t)
                     (re-search-forward "{" nil t)
                     (backward-char 1)
                     (forward-sexp))
                   (open-line 1)
                   (forward-char 1)
                   (insert (format "\\defonetext{%s}" name))))
           (if (not temp)
               (save-excursion (message "No defs found, insert in the above paragragh.")
                               (backward-paragraph)
                               (insert (format "\n\\defonetext{%s}" name)))))))

;;;###autoload
(defun two-def ()
  "insert deftwotext instead of def"
  (interactive)
  (let ((name (downcase (read-string "Enter macro name: ")))
        (body (buffer-substring-no-properties (mark) (point))))
    (if (use-region-p)
        (progn (kill-region (region-beginning) (region-end))
               (insert (format "\\%s" name))
               (save-excursion
                 (goto-char (point-min))
                 (setq temp (search-forward-regexp "^\\\\def" nil t))
                 (when temp
                   (message "Macro inserted.")
                   (beginning-of-line)
                   (while (re-search-forward "^\\\\def" nil t)
                     (re-search-forward "{" nil t)
                     (backward-char 1)
                     (forward-sexp))
                   (open-line 1)
                   (forward-char 1)
                   (insert (format "\\deftwotext{%s}{%s}" name body))))
               (if (not temp)
                   (save-excursion (message "No defs found, insert in the above paragragh.")
                                   (backward-paragraph)
                                   (insert (format "\n\\deftwotext{%s}{%s}" name body)))))
      (message "Please activate region which contains the definiton before inserting the def"))))

;;;###autoload
(defun get-defs ()
  "Collect all the defs in the tex document"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((res '()))
      (while (re-search-forward "^\\\\def" nil t)
        (let* ((start-pos (point))
               (s (cons
                   (buffer-substring-no-properties
                    (- (point) 4)
                    (progn
                      (re-search-forward "{" nil t)
                      (backward-char)
                      (cdr (bounds-of-thing-at-point 'sexp))))
                   start-pos)))
          (setq res (cons s res))))
      (nreverse res))))

;;;###autoload
(defun find-macro-name (x)
  "Find the name of a tex macro"
  (let* ((ind (string-match "{" x))
         (content (substring x 0 ind)))
    (cond
     ((string-equal content "\\defonetext")
      (concat "\\" (downcase (substring x (+ 1 ind) (string-match "}" x)))))
     ((string-equal content "\\deftwotext")
      (concat "\\" (downcase (substring x (+ 1 ind) (string-match "}" x)))))
     (t
      (substring x 4 ind)))))

;;;###autoload
(defun find-macro-content (x)
  "Find the content of a tex macro"
  (let* ((ind (string-match "{" x))
         (content (substring x 0 ind)))
    (cond
     ((string-equal content "\\defonetext")
      (substring x (1+ ind) -1))
     ((string-equal content "\\deftwotext")
      (substring x (1+ (string-match "{" x (1+ ind))) -1))
     (t
      (substring x (1+ ind) -1)))))

;;;###autoload
(defun tex-toggle-follow ()
  "Toggle tex-follow-up-or-not"
  (interactive)
  (setq tex-follow-up-or-not (not tex-follow-up-or-not)))

;; I shall aggregate this into the prefix keymap and combine with headlone,
;; so that I can type at a great speed.
;;;###autoload
(defun tex-follow-up ()
  "Follow the definition in the tex file"
  (interactive)
  (when tex-follow-up-or-not
    (with-ivy-window
      (goto-char (cdr (assoc (ivy-state-current ivy-last) tex-def-alist))))))

;;;###autoload
(defun read-tex-complete ()
  "my function to find all defs and use ivy as backend to complete it,
assuming all defs come at the beginning of line"
  (interactive)
  (setq tex-follow-up-or-not nil)
  (setq tex-def-alist (get-defs))
  (setq tex-old-pos (point))
  (ivy-read "defs: " (mapcar #'car tex-def-alist)
            :action '(1
                      ("o" (lambda (x)
                             (interactive)
                             (insert (format "%s" (find-macro-name x))))
                       "Insert Macro Name"))
            :update-fn #'tex-follow-up
            :unwind (lambda ()
                      (goto-char tex-old-pos)
                      (setq tex-def-alist nil))
            :keymap tex-def-map))

;;;###autoload
(defun make-blank-space (arg)
  "To make enough space to put something in. Default to up, with arg down"
  (interactive "P")
  (if (null arg)
      (progn
        (beginning-of-line)
        (open-line 3)
        (forward-line)
        (indent-according-to-mode))
    (progn
      (end-of-line)
      (open-line 3)
      (forward-line 2)
      (indent-according-to-mode))))

;; custom view function that deletes other windows
;;;###autoload
;; (defun durand-tex-view ()
;;   "Run `delete-other-windows' after `TeX-view'."
;;   (interactive)
;;   (TeX-view)
;;   (delete-other-windows))

;; I have to re-define this function in order to display the pdf in whole
;; window.
;;;###autoload
(defun durand-pdf-sync-forward-search (&optional line column)
  "Display the PDF location corresponding to LINE, COLUMN.
Modified by Durand."
  (interactive)
  (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
      (pdf-sync-forward-correlate line column)
    (let ((buffer (or (find-buffer-visiting pdf)
                      (find-file-noselect pdf))))
      (switch-to-buffer buffer nil t) ;; pdf-sync-forward-display-action
      (pdf-util-assert-pdf-window)
      (when page
        (pdf-view-goto-page page)
        (when y1
          (let ((top (* y1 (cdr (pdf-view-image-size)))))
            (pdf-util-tooltip-arrow (round top)))))
      (with-current-buffer buffer
        (run-hooks 'pdf-sync-forward-hook)))))

;; (set-popup-rule! "pdf$"
;;   :side 'bottom
;;   :size 0.5
;;   :select t
;;   :quit t)
