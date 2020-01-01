;;; lang/durand-greek/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'durand-transform-code-to-greek "lang/durand-greek/autoload" nil t)
(evil-define-operator durand-transform-code-to-greek (beg end type)
  "Transform code letters into ancient greek letters."
  :move-point nil
  :repeat t
  (interactive "<R>")
  (pcase type
    ('block
        (apply-on-rectangle 'durand-transform-each-in-rectangle beg end))
    (_
     (durand-greek-transform-from-beg-to-end beg end))))

;;;###autoload (autoload 'durand-greek-search-replace-generic "lang/durand-greek/autoload" nil nil 'macro)
(cl-defmacro durand-greek-search-replace-generic (match-str
                                                  replace
                                                  &key
                                                  (sub-exp 0)
                                                  (rep-exp 0)
                                                  str-p
                                                  search-end)
  "A macro to expand to a \"search and replace\" form.
MATCH-STR specifies the regexp to match.
REPLACE is the thing to replace the matched string.
If REPLACE is a string, it will replace the sub-expression specified by REP-EXP of the match.
If REPLACE is a list whose entries are lists consisting of two strings, then the replacing
string is (pcase (match-string SUB-EXP) REPLACE).
If REPLACE is a function, it will be applied to (match-string SUB-EXP) to produce the replacing
string.
STR-P means whether or not this operation is done for a string, or for buffer contents.
If STR-P is non-nil, then it should be a string, the string to search and replace.
SEARCH-END specifies the end of the search; this only has an effect when STR-P is nil."
  `(save-excursion
     (while (cond
             ((stringp ,str-p)
              (string-match ,match-str ,str-p))
             (t
              (re-search-forward ,match-str ,search-end t)))
       (cond ((stringp ,replace)
              (cond
               ((stringp ,str-p)
                (setq ,str-p (replace-match ,replace nil nil ,str-p ,rep-exp)))
               (t
                (replace-match ,replace nil nil nil ,rep-exp))))
             ((functionp ,replace)
              (cond
               ((stringp ,str-p)
                (setq
                 ,str-p
                 (replace-match (funcall ,replace (match-string ,sub-exp ,str-p)) nil nil ,str-p ,rep-exp)))
               (t
                (replace-match (funcall ,replace (match-string ,sub-exp ,str-p)) nil nil nil ,rep-exp))))
             ((listp ,replace)
              (cond
               ((stringp ,str-p)
                (setq ,str-p
                      (replace-match
                       (eval (append (list 'pcase (match-string ,sub-exp ,str-p)) ,replace))
                       nil nil ,str-p ,rep-exp)))
               (t
                (replace-match
                 (eval (append (list 'pcase (match-string ,sub-exp ,str-p)) ,replace))
                 nil nil nil ,rep-exp))))
             (t
              (user-error "Wrong type argument for REPLACE: either stringp, listp, or functionp; got %s"
                          (type-of ,replace)))))))

;;;###autoload
(defun durand-transform-string (str)
  "Transform the string containing code letters to ancient greek letters."
  (dolist (spec durand-greek-replace-list str)
    (durand-greek-search-replace-generic (car spec)
                                         (cadr spec)
                                         :sub-exp (or (plist-get (cddr spec) :sub-exp) 0)
                                         :rep-exp (or (plist-get (cddr spec) :rep-exp) 0)
                                         :str-p str
                                         :search-end (plist-get (cddr spec) :search-end))))

;;;###autoload
(defun durand-greek-transform-from-beg-to-end (beg end)
  "Transform the code letters into ancient greek letters on the region from BEG to END."
  (let ((orig-marker (make-marker))
        (end-marker (make-marker)))
    (message "transforming code letters...")
    (set-marker orig-marker (point))
    (set-marker end-marker end)
    (goto-char beg)
    (dolist (spec durand-greek-replace-list)
      (durand-greek-search-replace-generic (car spec)
                                           (cadr spec)
                                           :sub-exp (or (plist-get (cddr spec) :sub-exp) 0)
                                           :rep-exp (or (plist-get (cddr spec) :rep-exp) 0)
                                           :str-p nil
                                           :search-end (marker-position end-marker)))
    (goto-char (marker-position orig-marker))
    (set-marker orig-marker nil)
    (set-marker end-marker nil)
    (message "transforming code letters...DONE")))

;;;###autoload
(defun durand-transform-each-in-rectangle (startcol endcol)
  "Starting from startcol, transform to endcol."
  (let ((orig-marker (make-marker))
        (end-marker (make-marker))
        start)
    (message "transforming code letters...")
    (set-marker orig-marker (point))
    (move-to-column startcol)
    (setq start (point))
    (move-to-column endcol)
    (set-marker end-marker (point))
    (goto-char start)
    (dolist (spec durand-greek-replace-list)
      (durand-greek-search-replace-generic (car spec)
                                           (cadr spec)
                                           :sub-exp (or (plist-get (cddr spec) :sub-exp) 0)
                                           :rep-exp (or (plist-get (cddr spec) :rep-exp) 0)
                                           :str-p nil
                                           :search-end (marker-position end-marker)))
    (goto-char (marker-position orig-marker))
    (set-marker orig-marker nil)
    (set-marker end-marker nil)
    (message "transforming code letters...DONE")))

;;;###autoload
(defun durand-search-greek (input &optional already-transformed)
  "Search the buffer with the input being interpreted as code letters for greek letters.
The search is insensitive to accent marks and capitalisation.
If ALREADY-TRANSFORMED is non-nil, then don't perform transformation."
  (interactive (list (read-from-minibuffer "Search for: ")))
  (let* ((decoded-input (durand-transform-string input))
         (insensitive-input
          (cond
           ((null already-transformed)
            (cl-mapc
             (lambda (x y)
               (let ((start 0))
                 (while (setf start (string-match x decoded-input start))
                   (setf decoded-input (replace-match y nil nil decoded-input)
                         start (+ start (length y))))))
             '("α" "ε" "ι" "ο" "υ" "η" "ω" "ρ")
             '("[αάὰἁἀἅἄἃἂᾅᾄᾃᾂᾶἇἆᾇᾆᾳ]"
               "[εέὲἑἐἕἔἓἒ]"
               "[ιίὶἱἰἵἴἳἲῖἷἶϊΐῒῗ]"
               "[οόὸὁὀὅὄὃὂ]"
               "[υύὺὑὐὕὔὓὒῦὗὖϋΰῢῧ]"
               "[ηἠἡήἤἥὴἢἣῆἦἧῃᾐᾑῄᾔᾕῂᾒᾓῇᾖᾗ]"
               "[ωὠὡώὤὥὼὢὣῶὦὧῳᾠᾡῴᾤᾥῲᾢᾣῷᾦᾧ]"
               "[ρῥ]"))
            decoded-input)
           (t input)))
         (orig (point)))
    (push insensitive-input durand-search-greek-last-search)
    (unless (re-search-forward insensitive-input nil t)
      (goto-char (point-min))
      (unless (re-search-forward insensitive-input nil t)
        (goto-char orig)))
    (dolist (ov durand-search-greek-overlays)
      (delete-overlay ov)
      (setf durand-search-greek-overlays nil))
    (when (/= orig (point))
      (let ((ov (make-overlay (match-beginning 0)
                              (match-end 0))))
        (overlay-put ov 'face isearch-face)
        (push ov durand-search-greek-overlays)))))

;;;###autoload
(defun durand-show-search-greek (input &optional already-transformed)
  "Search the buffer with the input being interpreted as code letters for greek letters.
The search is insensitive to accent marks and capitalisation.
If ALREADY-TRANSFORMED is non-nil, then don't perform transformation."
  (interactive (list (read-from-minibuffer "Search for: ")))
  (let* ((decoded-input (durand-transform-string input))
         (insensitive-input
          (cond
           ((null already-transformed)
            (cl-mapc
             (lambda (x y)
               (let ((start 0))
                 (while (setf start (string-match x decoded-input start))
                   (setf decoded-input (replace-match y nil nil decoded-input)
                         start (+ start (length y))))))
             '("α" "ε" "ι" "ο" "υ" "η" "ω" "ρ")
             '("[αάὰἁἀἅἄἃἂᾅᾄᾃᾂᾶἇἆᾇᾆᾷᾳ]"
               "[εέὲἑἐἕἔἓἒ]"
               "[ιίὶἱἰἵἴἳἲῖἷἶϊΐῒῗ]"
               "[οόὸὁὀὅὄὃὂ]"
               "[υύὺὑὐὕὔὓὒῦὗὖϋΰῢῧ]"
               "[ηἠἡήἤἥὴἢἣῆἦἧῃᾐᾑῄᾔᾕῂᾒᾓῇᾖᾗ]"
               "[ωὠὡώὤὥὼὢὣῶὦὧῳᾠᾡῴᾤᾥῲᾢᾣῷᾦᾧ]"
               "[ρῥ]"))
            decoded-input)
           (t input)))
         res)
    (push insensitive-input durand-search-greek-last-search)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward insensitive-input nil t)
        (push (list (buffer-substring
                     (line-beginning-position)
                     (line-end-position))
                    (match-beginning 0)
                    (match-end 0)
                    (line-beginning-position))
              res))
      (setf res (cl-remove-duplicates
                 (nreverse res)
                 :test (lambda (x y) (= (cadddr x) (cadddr y))))))
    (dolist (ov durand-search-greek-overlays)
      (delete-overlay ov)
      (setf durand-search-greek-overlays nil))
    (dolist (item res)
      (let ((ov (make-overlay (cadr item)
                              (caddr item))))
        (overlay-put ov 'face isearch-face)
        (push ov durand-search-greek-overlays)))
    (with-current-buffer-window
     "*Search-results*" nil nil
     (dolist (item res)
       (let ((text (propertize (car item) 'pos (caddr item))))
         (insert text)
         (insert "\n")))
     (durand-greek-search-mode))
    (other-window 1)
    (durand-goto-greek-search-result (caddar res))))

;;;###autoload
(defun durand-goto-greek-search-result (&optional pos)
  "Go to the position corresponding to the search result at point."
  (interactive)
  (let ((dest (or pos
                  (get-text-property (point) 'pos)
                  (user-error "No \"pos\" information at point"))))
    (other-window 1)
    (goto-char dest)
    (org-show-context)
    (other-window 1)))

;;;###autoload
(defun durand-clear-overlays ()
  "Clear overlays"
  (interactive)
  (dolist (ov durand-search-greek-overlays)
    (delete-overlay ov)
    (setf durand-search-greek-overlays nil)))

;;;###autoload
(defun durand-search-last-greek ()
  "Search the last searched term."
  (interactive)
  (cond
   (durand-search-greek-last-search
    (durand-search-greek
     (pop durand-search-greek-last-search)
     t))
   (t
    (message "No last searched term."))))

;; ARCHIVE


;; (defmacro flat-pcase (str ls)
;;   "Plug LS in a pcase form against STR."
;;   (unless (listp ls)
;;     (user-error "Wrong type argument for LS, listp, got %s" (type-of ls)))
;;   `(pcase ,str
;;      ,@ls))
