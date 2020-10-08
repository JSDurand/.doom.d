;;; lang/durand-org/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun durand-redo-agenda ()
  "Redo and then go to the first block."
  (interactive)
  (org-agenda-redo-all)
  (evil-emacs-state)
  (org-agenda-first-block))

;;;###autoload
(defun durand-agenda-advice (func &rest arg)
  "Advice the org-agenda functions so that it goes to the first
  block after it finishes."
  (interactive)
  (widen)
  (funcall func (or (car arg) 1))
  (evil-emacs-state)
  (org-agenda-first-block))

;;;###autoload
(defun durand-agenda-advice-for-today (func)
  "Advice for org-agenda-goto-today."
  (interactive)
  (widen)
  (funcall func)
  (evil-emacs-state)
  (org-agenda-first-block))

;; fix the issue that org does not kill a buffer if I touched it.

(defadvice! durand-org-readd-buffer-to-kill (&rest _args)
  "Re-add the buffer to be killed."
  :after '+org--restart-mode-h
  (push (current-buffer) org-agenda-new-buffers))

;; fix org-agenda-later
;;;###autoload
(defun org-agenda-later (arg)
  "Go forward in time by the current span.
With prefix ARG, go forward that many times the current span."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
         (span (or (nth 2 args) org-agenda-current-span))
         (sd (or (nth 1 args) (org-get-at-bol 'day) org-starting-day))
         (greg (calendar-gregorian-from-absolute sd))
         (cnt (org-get-at-bol 'org-day-cnt))
         greg2)
    (cond
     ((numberp span)
      (setq sd (+ (* span arg) sd)))
     ((eq span 'day)
      (setq sd (+ arg sd)))
     ((eq span 'week)
      (setq sd (+ (* 7 arg) sd)))
     ((eq span 'fortnight)
      (setq sd (+ (* 14 arg) sd)))
     ((eq span 'month)
      (setq greg2 (list (+ (car greg) arg) (nth 1 greg) (nth 2 greg))
            sd (calendar-absolute-from-gregorian greg2))
      (setcar greg2 (1+ (car greg2))))
     ((eq span 'year)
      (setq greg2 (list (car greg) (nth 1 greg) (+ arg (nth 2 greg)))
            sd (calendar-absolute-from-gregorian greg2))
      (setcar (nthcdr 2 greg2) (1+ (nth 2 greg2))))
     (t
      (setq sd (+ (* span arg) sd))))
    (let ((org-agenda-overriding-cmd
           ;; `cmd' may have been set by `org-agenda-run-series' which
           ;; uses `org-agenda-overriding-cmd' to decide whether
           ;; overriding is allowed for `cmd'
           (get-text-property (min (1- (point-max)) (point)) 'org-series-cmd))
          (org-agenda-overriding-arguments
           (list (car args) sd span)))
      (ignore org-agenda-overriding-cmd
              org-agenda-overriding-arguments)
      (org-agenda-redo)
      ;; I added the following line.
      (org-agenda-first-block)
      (org-agenda-find-same-or-today-or-agenda cnt))))

;; Fix org-agenda-goto-today

;;;###autoload
(defun org-agenda-goto-today ()
  "Go to today."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
         (curspan (nth 2 args))
         (tdpos (text-property-any (point-min) (point-max) 'org-today t)))
    (cond
     (tdpos (goto-char tdpos))
     ((eq org-agenda-type 'agenda)
      (let* ((sd (org-agenda-compute-starting-span
                  (org-today) (or curspan org-agenda-span)))
             (org-agenda-overriding-arguments args))
        (setf (nth 1 org-agenda-overriding-arguments) sd)
        (org-agenda-redo)
        ;; I added the following line.
        (org-agenda-first-block)
        (org-agenda-find-same-or-today-or-agenda)))
     (t (error "Cannot find today")))))

;;;###autoload
(defun durand-agenda-exit ()
  "Execute `general-hydra/body' after `org-agenda-exit'.
Don't bind it to a key in `general-hydra/heads'"
  (interactive)
  (org-agenda-exit))

;; Recenter to top
;;;###autoload
(defun recenter-to-top (&rest _some)
  "Recenter to top"
  (interactive)
  (recenter 0))

;;; Pick the favourite items in the list
;;;
;;; NOTE: An item is said to be favourite if and only if it appears more than
;;; `durand-fav-threshold' of the times in the list.

;;;###autoload
(defvar durand-fav-threshold 0
  "The threshold that determines when an item is a favourite.")

(setf durand-fav-threshold 0.9)

;;;###autoload
(defun durand-pick-favs (items)
  "Pick the favourite items in the list.
ITEMS is a list of lists of strings, and an item is said to be a
favourite if and only if it appears more than
`durand-fav-threshold' of the times in the list."
  (let* ((total (length items))
         (items-and-freqs
          (cl-loop with result
                   for items-in-entry in items
                   do
                   (cl-loop
                    for item in items-in-entry
                    do
                    (setf (alist-get item result nil nil #'string=)
                          (let ((asso (alist-get item result nil nil #'string=)))
                            (1+ (or asso 0)))))
                   finally return result)))
    (cl-loop for (item . freq) in items-and-freqs
             if (>= freq (floor (* total durand-fav-threshold)))
             collect item)))

;;; completion for accounts

;;;###autoload
(defun durand-org-complete-capture-account ()
  "Offer completion for shops and items and costs when capturing accounts."
  (with-current-file (expand-file-name "account/account.org" org-directory) nil
    (let* ((shops-list (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (point-min))
                           (cl-loop while (re-search-forward org-heading-regexp nil t)
                                    for title = (save-excursion
                                                  (org-end-of-meta-data)
                                                  (org-skip-whitespace)
                                                  (buffer-substring-no-properties
                                                   (point) (line-end-position)))
                                    if (and (= (car (org-heading-components)) 4)
                                            title
                                            (not (string= title "")))
                                    collect title))))
           (shops-list (nreverse
                        (cl-remove-duplicates
                         (nreverse
                          (append durand-frequent-shops
                                  (remove nil shops-list)))
                         :key 'upcase
                         :test 'string=)))
           (chosen-shop (completing-read
                         "Chois un magasin: " shops-list
                         nil nil nil durand-complete-shop-history))
           (items-list
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (cl-loop
                 named parent
                 ;; with items
                 while (re-search-forward org-heading-regexp nil t)
                 for title = (save-excursion
                               (org-end-of-meta-data)
                               (org-skip-whitespace)
                               (buffer-substring-no-properties
                                (point) (line-end-position)))
                 if (and (= (car (org-heading-components)) 4)
                         title
                         (not (string= title ""))
                         (string= (upcase title)
                                  (upcase chosen-shop)))
                 append (list
                         (cl-loop
                          named child
                          with limite = (save-excursion (outline-next-heading) (point))
                          while (re-search-forward "- " limite t)
                          collect (buffer-substring-no-properties
                                   (point) (line-end-position))))

                 ;; REVIEW: The code below is to collect the items I ordered the
                 ;; last time. Since I now want to analyse the frequencies of
                 ;; the ordered items, I commented the code below.
                 ;;
                 ;; do (setf items (cl-loop
                 ;;                 named child
                 ;;                 with limite = (save-excursion (outline-next-heading) (point))
                 ;;                 while (re-search-forward "- " limite t)
                 ;;                 collect (buffer-substring-no-properties
                 ;;                          (point) (line-end-position))))
                 ;; and append items into all-items
                 ;; finally return (list all-items items)
                 ))))
           (durand-choose-list-result (durand-pick-favs items-list))
           (items-list (cl-remove-duplicates (-flatten items-list) :test 'string=))
           (chosen-items (durand-choose-list (mapcar 'list items-list) nil "Chois une chose: " t nil t
                                             nil t)))
      (concat chosen-shop
              "\n\n"
              (cl-loop for item in chosen-items
                       unless (string= item "")
                       concat (concat "- " item "\n"))
              ;; REVIEW: Using `string-join' seems to be better suited to this
              ;; job. But it will not exclude empty strings automatically, so
              ;; using a loop is fine from my view point.
              ;;
              ;; (string-join chosen-items "\n- ")
              ;; "\n"
              ))))

;; Find the next link
;;;###autoload
(defun durand-find-next-link (&optional pos func)
  "Find the next link. POS defaults to `(point)' and FUNC defaults to `next-single-property-change'."
  (let ((pos (or pos (point)))
        (func (or func 'next-single-property-change))
        res)
    (dolist (le_type durand-link-types res)
      (setq res (or res (funcall func pos le_type))))))

;;;###autoload
(defun durand-find-previous-link (&optional pos func)
  "Find the previous link. POS defaults to `(point)' and FUNC defaults to `previous-single-property-change'."
  (let ((pos (or pos (point)))
        (func (or func 'previous-single-property-change))
        res)
    (dolist (le_type durand-link-types res)
      (setq res (or res (funcall func pos le_type))))))

;;;###autoload
(defun durand-forward-link ()
  (interactive)
  (let* ((next-change (durand-find-next-link))
         (next-url (when next-change (durand-find-next-link next-change 'get-text-property)))
         (final-change (if next-url next-change (durand-find-next-link next-change))))
    (if final-change
        (goto-char final-change)
      (message "No links found!"))))

;;;###autoload
(defun durand-backward-link ()
  (interactive)
  (let* ((next-change (durand-find-previous-link))
         (next-url (when next-change (durand-find-previous-link next-change 'get-text-property)))
         (final-change (if next-url next-change (durand-find-previous-link next-change))))
    (if final-change
        (goto-char final-change)
      (message "No links found!"))))

;;;###autoload
(defun orgy-view ()
  "Recenter to top; if already there, return to previous position"
  (interactive)
  (let ((window-line (count-visible-lines (window-start) (point))))
    (if (= window-line (1+ scroll-margin))
        (recenter (or (1- (get 'orgy-recenter :line)) 0))
      (put 'orgy-recenter :line window-line)
      (recenter 0))))

;;;###autoload
(defun count-visible-lines (beg end)
  "Count visible lines between BEG and END"
  (interactive)
  (let ((line-num 1))
    (goto-char beg)
    (while (/= (line-number-at-pos (point)) (line-number-at-pos end))
      (forward-visible-line 1)
      (setq line-num (1+ line-num)))
    line-num))

;;;###autoload
(defun durand-org-back-to-repeat ()
  (interactive)
  (when (string= org-state "REPEATED")
    (org-set-property "LAST-REPEAT" (format-time-string (org-time-stamp-format t t) (current-time)))
    (org-add-log-setup 'state "REPEATED" "TO-REPEAT" 'time)
    (org-todo "TO-REPEAT")))

;;;###autoload
(defun org-protocol-convert-query-to-plist (query)
  "Convert QUERY key=value pairs in the URL to a property list."
  (when query
    (apply 'append
           (mapcar
            (lambda (x)
              (let ((c (split-string x "=")))
                (list
                 (intern (concat ":" (car c)))
                 (mapconcat #'identity (cdr c) "="))))
            (split-string query "&")))))

;;;###autoload
(defadvice! durand-org-protocol-convert-query (query)
  "Convert QUERY key=value pairs in the URL to a property list.
Modified by Durand to handle youtube links correctly."
  :override 'org-protocol-convert-query-to-plist
  (when query
    (apply 'append
           (mapcar
            (lambda (x)
              (let ((c (split-string x "=")))
                (list
                 (intern (concat ":" (car c)))
                 (mapconcat #'identity (cdr c) "="))))
            (split-string query "&")))))

;; filter out the title
;;;###autoload
(defun org-filter-title ()
  "Filter out some unnecessary parts of the link title"
  (let ((title (plist-get org-store-link-plist :description)))
    (cond
     ((string-match " - Mathematics Stack Exchange" title)
      (replace-match "" nil nil title))
     ((string-match " - YouTube" title)
      (replace-match "" nil nil title))
     ((string-match "\\(.*?\\)最新章节列表,\\1无弹窗_UU看书" title)
      (replace-match "\\1" nil nil title))
     (t
      title))))

;;;###autoload
(defun durand-org-set-store-link (link description selection)
  "Do the works of `org-store-link' for weblinks.
Since `org-protocol' does not handle some web links, I decided to
make my own functions."
  (let* ((type (substring link
                          0
                          (-find-index (lambda (x) (= x ?\:))
                                       (string-to-list link))))
         (annotation (org-make-link-string link description))
         (query (list :template "L"
                      :url link
                      :title description
                      :body selection)))
    (setf org-store-link-plist
          (list :type type
                :link link
                :description description
                :annotation annotation
                :initial ""
                :query query))
    (push (list link description) org-stored-links)))

;; filter title in the link
;;;###autoload
(defun org-filtered-link ()
  "Filter out some unnecessary parts in the link description"
  (let* ((link (plist-get org-store-link-plist :link))
         (title (plist-get org-store-link-plist :description))
         (filtered (cond ((string-match " - Mathematics Stack Exchange" title)
                          (replace-match "" nil nil title))
                         ((string-match " - YouTube" title)
                          (replace-match "" nil nil title))
                         ((string-match "\\(.*?\\)最新章节列表,\\1无弹窗_UU看书" title)
                          (replace-match "\\1" nil nil title))
                         (t
                          title))))
    (org-make-link-string link filtered)))


;; Determine tag based upon the URL
;;;###autoload
(defun org-determine-tag ()
  "Determine tag based upon the URL"
  (let ((link (plist-get org-store-link-plist :link)))
    (cond
     ((string-match "https?://www.youtube.com" link)
      ":youtube:")
     ((or
       (string-match "https?://math.stackexchange.com" link)
       (string-match "https?://mathoverflow.net/" link))
      ":stack:web_link:")
     ((cl-some
       (lambda (re) (string-match re link))
       durand-novel-addresses-regexp)
      ":roman:")
     ((string-match "https?://stacks.math.columbia.edu/" link)
      ":web_link:stack:")
     (t
      ":web_link:"))))

;;;###autoload
(defun org-determine-link-file ()
  "Go to the file to capture to based upon the URL"
  (let* ((link (plist-get org-store-link-plist :link))
         (file-name
          (cond
           ((string-match "https?://www.youtube.com" link)
            "youtube_links.org")
           ((or
             (string-match "https?://math.stackexchange.com" link)
             (string-match "https?://mathoverflow.net/" link))
            "math_article_links.org")
           ((cl-some
             (lambda (re) (string-match re link))
             durand-novel-addresses-regexp)
            "notes.org")
           ((string-match "https?://stacks.math.columbia.edu/" link)
            "math_article_links.org")
           (t
            "notes.org"))))
    (find-file (expand-file-name file-name org-directory))
    (goto-char (point-max))))

;;; kill server buffer
;;;
;;; FIXME: This does not work. It even hinders the execution of the external
;;; command.

;;;###autoload
;; (defadvice! durand-bury-server-buffer ()
;;   "Bury the server buffer to avoid annoiance."
;;   :after 'org-update-novels
;;   (when (get-buffer " *server*")
;;     (bury-buffer (get-buffer " *server*"))))

;; automatically update account when capturing
;;;###autoload
(defun durand-capture-update-account ()
  "Run org-update-account if the template is \"m\""
  (interactive)
  (when (equal (plist-get org-capture-plist :key) "m")
    (with-current-buffer "account.org"
      (org-update-account)
      (ignore-errors (save-buffer 0))
      (durand-show-account-report))))

;;;###autoload
(defvar durand-capture-wconf nil
  "Window configuration to return to after capture process")

;;;###autoload
(defvar durand-capture-go-to-buffer nil
  "buffer to go to after capture process")

;;;###autoload
(defun durand-capture-set-window-conf ()
  "Set up the window configuration and buffer for later use."
  (setf durand-capture-wconf (org-capture-get :return-to-wconf 'local)
        durand-capture-go-to-buffer (org-capture-get :buffer 'local)))

;;;###autoload
(defun durand-capture-reposition-windows ()
  "Reposition the windows so that there are no pop-ups anymore."
  (let ((conf durand-capture-wconf)
        (buf durand-capture-go-to-buffer))
    (when (ignore-errors (window-configuration-p conf))
      (set-window-configuration conf))
    (when (ignore-errors (bufferp buf))
      (switch-to-buffer buf))))

;;;###autoload
(defun durand-collect-shop-infos ()
  "Return relevant information from the heading."
  (when (= (car (org-heading-components)) 4)
    ;; It is possible to be called inside `org-map-entries'.
    (let* ((date-string (if (save-excursion
                              (outline-up-heading 1 t)
                              (re-search-forward org-date-tree-headline-regexp (line-end-position) t))
                            (match-string-no-properties 1)
                          (user-error "No matching date found!")))
           (title
            (save-excursion
              (org-end-of-meta-data)
              (org-skip-whitespace)
              (buffer-substring-no-properties (point) (line-end-position))))
           (cost (org-entry-get (point) "cost"))
           (from-string (org-entry-get (point) "from"))
           (from (when from-string
                   (let ((ori (split-string from-string "\\( \\|:\\)"))
                         res)
                     (dolist (ele ori)
                       (unless (numberp (read ele))
                         (push ele res)))
                     (setf res (nreverse res))
                     (dotimes (i (length ori) (progn (ignore i) res))
                       (when (numberp (read (nth i ori)))
                         (if (= i 0)
                             (user-error "The first element of FROM cannot be a number!")
                           (setf res (remove (nth (1- i) ori) res)
                                 res (append res
                                             (list (cons (nth (1- i) ori)
                                                         (read (nth i ori))))))))))))
           (balanced-from (when from-string
                            (let (temp ave (cur 0))
                              (dolist (ele from)
                                (cond
                                 ((consp ele)
                                  (setf cur (+ cur (cdr ele))))
                                 ((stringp ele)
                                  (push ele temp))
                                 (t
                                  (user-error "ELE is strange: %s" ele))))
                              (setf ave (/ (- (read cost) cur) (float (length temp))))
                              (dolist (ele temp from)
                                (setf from (remove ele from)
                                      from (append from
                                                   (list (cons ele ave)))))))))
      (dolist (ele balanced-from balanced-from)
        (setf balanced-from (remove ele balanced-from)
              balanced-from (push (cons (car ele)
                                        (- (cdr ele)))
                                  balanced-from)))
      (list date-string title cost balanced-from))))

;;;###autoload
(defun durand-show-account-report (&optional period-func report-mode sum-type exclude-type)
  "Show a report of account.
PERIOD-FUNC should take an argument of date string, and return
true only when that date is under consideration.
By default PERIOD-FUNC specifies the last day only.
REPORT-MODE can be either SEPARATE or COMBINE.
SUM-TYPE can be ALL or a regexp matching what would be summed.
EXCLUDE-TYPE can be nil or a regexp matching what would not be summed."
  (interactive)
  (with-account
   (save-excursion
     (goto-char (point-min))
     (let ((period-func (or period-func 'durand-account-match-last-unit))
           (report-mode (or report-mode 'separate))
           (sum-type (or sum-type 'all))
           (exclude-type (cond ((or (null exclude-type) (string= exclude-type ""))
                                "Cash\\|CTBC-bank-account\\|etique")
                               (t
                                exclude-type)))
           infos combined)
       (while (re-search-forward org-date-tree-headline-regexp nil t)
         (when (funcall period-func (match-string-no-properties 1))
           (setf infos
                 (append
                  infos
                  (remove nil
                          (org-map-entries #'durand-collect-shop-infos nil 'tree))))))
       (cond
        ((eq report-mode 'combine)
         (dolist (ele infos)
           (let* ((date (read (car ele)))
                  (date-list (plist-get combined date))
                  (tit (intern (cadr ele)))
                  (cur (or (plist-get date-list tit) 0))
                  (val (read (caddr ele))))
             (setf combined (plist-put combined
                                       date
                                       (plist-put date-list tit (+ val cur)))))
           (let* ((date (intern (car ele)))
                  (date-list (plist-get combined date)))
             (dolist (exp (cadddr ele))
               (let ((cur (or (plist-get date-list (read (car exp))) 0))
                     (tit (intern (car exp)))
                     (val (cdr exp)))
                 (setf combined (plist-put combined
                                           date
                                           (plist-put date-list tit (+ val cur)))))))))
        ((eq report-mode 'separate)
         (dolist (ele infos)
           (let* ((date (read (car ele)))
                  (date-list (plist-get combined date))
                  (tit (intern (cadr ele)))
                  (val (read (caddr ele))))
             (setf combined (plist-put combined
                                       date
                                       (append date-list (list tit val)))))
           (let* ((date (intern (car ele)))
                  (date-list (plist-get combined date)))
             (dolist (exp (cadddr ele))
               (let ((tit (intern (car exp)))
                     (val (cdr exp)))
                 (setf combined (plist-put combined
                                           date
                                           (append date-list (list tit val)))))))))
        (t
         (user-error "Unknown report mode: %s" report-mode)))
       (with-current-buffer-window
        "*ACCOUNT REPORT*" nil nil
        (insert (format "%s\nREPORT MODE: %s\nSUM-TYPE: %s\nEXCLUDE-TYPE: %s\n%s\n" durand-account-report-period-str
                        report-mode sum-type exclude-type
                        (make-string (- (window-width) 2) ?-)))
        (let ((all-total 0))
          (dotimes (i (/ (length combined) 2))
            (let ((day-total 0)
                  (date (nth (* 2 i) combined))
                  (date-info (nth (1+ (* 2 i)) combined)))
              (insert (format "%s:\n" date))
              (dotimes (j (/ (length date-info) 2))
                (when (and (or (eq sum-type 'all)
                               (string-match sum-type (format "%s" (nth (* 2 j) date-info))))
                           (or (eq exclude-type 'nothing)
                               (not (string-match exclude-type (format "%s" (nth (* 2 j) date-info))))))
                  (cl-incf all-total (nth (1+ (* 2 j)) date-info))
                  (cl-incf day-total (nth (1+ (* 2 j)) date-info)))
                (insert (format "  %s: %s\n"
                                (nth (* 2 j) date-info)
                                (nth (1+ (* 2 j)) date-info))))
              (insert (format "  %s: %s\n  %s: %s\n"
                              'day-total day-total
                              'all-total all-total)))))
        (account-report-mode))
       ;; (select-window (get-buffer-window "*ACCOUNT REPORT*"))
       ;; (delete-other-windows)
       ))))

;;;###autoload
(defun durand-date-to-time (str)
  "Convert a date string STR to time.
Date string should separated by either space, dash, or underline."
  (let* ((splitted (split-string str "[ |_|-]+"))
         (splitted-list (mapcar #'string-to-number splitted))
         (time
          (append
           (list 0 0 0
                 (caddr splitted-list)
                 (cadr splitted-list)
                 (car splitted-list))
           (nthcdr 6 (decode-time)))))
    (encode-time time)))

;;;###autoload
(defun durand-account-normalize-time (time-value)
  "Normalize TIME-VALUE.
The result will have second, minute, and hour all equal to 0."
  (let* ((decoded (decode-time time-value)))
    (encode-time
     (append
      (list 0 0 0)
      (nthcdr 3 decoded)))))

;;;###autoload
(defun durand-account-time-p (x)
  "Return t if X is a time object.
A time object is a list of the form (sec-high sec-low microsec picosec)
The formula is SEC = sec-high * 2^{16} + sec-low + microsec * 10^{-6} + picosec * 10^{-12}."
  (let ((l (length x)))
    (and (listp x)
         (>= l 2)
         (<= l 4)
         (=
          (cl-loop for element in x
                   sum (cond ((integerp element)
                              1)
                             (t 0)))
          l))))

;;;###autoload
(defun durand-account-format-time (time)
  "Format TIME in a pretty and unified way."
  (cl-assert (durand-account-time-p time))
  (let ((time (decode-time time)))
    (format
     "%d-%s-%s"
     (decoded-time-year time)
     (pad-string-to (number-to-string (decoded-time-month time)) 2)
     (pad-string-to (number-to-string (decoded-time-day time)) 2))))

;;;###autoload
(cl-defun durand-account-format-unit (&optional (unit 'day))
  "Format the UNIT in a pretty and unified way."
  (cond
   ((null unit) "day")
   ((symbolp unit) (symbol-name unit))
   ((and (listp unit)
         (= (length unit) 2)
         (durand-account-time-p (car unit))
         (durand-account-time-p (cadr unit)))
    (let* ((start-value (car unit))
           (end-value (cadr unit)))
      (string-join
       (list
        (durand-account-format-time start-value)
        (durand-account-format-time end-value))
       " to ")))
   (t
    (user-error "Not supported unit: %S" unit))))

;;;###autoload
(defun durand-account-match-last-unit (str &optional unit)
  "Match the last UNIT. UNIT can be `day', `week', `month', `year',
or a custom specifier of time period."
  (setf durand-account-report-period-str (durand-account-format-unit unit))
  (with-account
   (let* ((last-time-str (caar (last (org-find-all-days))))
          (last-time (durand-date-to-time last-time-str))
          (last-list (decode-time last-time))
          (last-year (nth 5 last-list))
          (last-month (nth 4 last-list))
          (last-day (nth 3 last-list))
          (str-time (durand-date-to-time str))
          (str-list (decode-time str-time))
          (str-year (nth 5 str-list))
          (str-month (nth 4 str-list))
          (str-day (nth 3 str-list)))
     ;; Ensure that the time being matched is less than or equal to the last day
     (cl-assert (or (time-less-p str-time last-time)
                    (and (= last-day str-day)
                         (= last-month str-month)
                         (= last-year str-year))))
     (pcase unit
       ((or (pred null) 'day)
        (and (= last-day str-day)
             (= last-month str-month)
             (= last-year str-year)))
       ('week
        (>= (time-to-days str-time)
            (- (time-to-days last-time) 7)))
       ('month
        (and (= last-month str-month)
             (= last-year str-year)))
       ('year
        (= last-year str-year))
       ((pred
         (lambda (x)
           (and (listp x)
                (= (length x) 2)
                (durand-account-time-p (car x))
                (durand-account-time-p (cadr x)))))
        (let* ((beg (durand-account-normalize-time (car unit)))
               (end (durand-account-normalize-time (cadr unit))))
          (and (or (time-less-p beg str-time)
                   (time-equal-p beg str-time))
               (or (time-less-p str-time end)
                   (time-equal-p str-time end)))))
       ;; ((pred stringp)
       ;;  (let* ((str-list (split-string unit ":"))
       ;;         (beg-str (car str-list))
       ;;         (end-str (cond
       ;;                   ((not (string= (cadr str-list) ""))
       ;;                    (cadr str-list))
       ;;                   (t
       ;;                    "+0")))
       ;;         (beg (durand-account-normalize-time
       ;;               (org-read-date nil t beg-str "Chois le début:")))
       ;;         (end (durand-account-normalize-time
       ;;               (org-read-date nil t end-str "Chois la fin:"))))
       ;;    (and (or (time-less-p beg str-time)
       ;;             (time-equal-p beg str-time))
       ;;         (or (time-less-p str-time end)
       ;;             (time-equal-p str-time end)))))
       (_
        (user-error "Unknown unit: %S" unit))))))

;; convenient functions

;;;###autoload
(defvar durand-account-report-time-regexp
  (format "\\(%s\\)-\\(%s\\)-\\(%s\\)"
          "[[:digit:]]\\{4\\}"
          "[[:digit:]]\\{2\\}"
          "[[:digit:]]\\{2\\}")
  "The regular expression to match the custom time specifier in
  the report of account.")

;;;###autoload
(cl-defun durand-change-parameter (&key unit report-mode sum-type exclude-type)
  "general function to change the parameters of account reporting"
  (let* ((account-buffer-name "account.org")
         cur-u cur-rm cur-st cur-et)
    (when (get-buffer "*ACCOUNT REPORT*")
      (switch-to-buffer "*ACCOUNT REPORT*")
      (goto-char (point-min))
      (setf cur-u (when (null unit)
                    (let* ((str (buffer-substring-no-properties (point) (line-end-position))))
                      (cond ((string= str "day") (intern str))
                            ((string= str "week") (intern str))
                            ((string= str "month") (intern str))
                            ((string= str "year") (intern str))
                            ((string-match
                              (format
                               "%s to %s"
                               durand-account-report-time-regexp
                               durand-account-report-time-regexp)
                              str)
                             (let ((match-one (match-string-no-properties 1 str))
                                   (match-two (match-string-no-properties 2 str))
                                   (match-three (match-string-no-properties 3 str))
                                   (match-four (match-string-no-properties 4 str))
                                   (match-five (match-string-no-properties 5 str))
                                   (match-six (match-string-no-properties 6 str)))
                               (list (durand-date-to-time
                                      (format "%s-%s-%s"
                                              match-one
                                              match-two
                                              match-three))
                                     (durand-date-to-time
                                      (format "%s-%s-%s"
                                              match-four
                                              match-five
                                              match-six)))))
                            (t str))))
            cur-rm (intern (progn
                             (forward-line)
                             (buffer-substring-no-properties (+ 13 (point)) (line-end-position))))
            cur-st (let ((st (progn
                               (forward-line)
                               (buffer-substring-no-properties (+ 10 (point)) (line-end-position)))))
                     (cond ((string= st "all") 'all)
                           (t st)))
            cur-et (let ((et (progn
                               (forward-line)
                               (buffer-substring-no-properties (+ 14 (point)) (line-end-position)))))
                     (cond ((string= et "nothing") 'nothing)
                           (t et)))))
    (cond ((get-buffer account-buffer-name) (switch-to-buffer account-buffer-name))
          (t (find-file (expand-file-name
                         account-buffer-name
                         (expand-file-name "account" org-directory)))))
    (durand-show-account-report (lambda (str)
                                  (durand-account-match-last-unit
                                   str (or unit cur-u)))
                                (or report-mode cur-rm)
                                (or sum-type cur-st)
                                (or exclude-type cur-et))))

;;;###autoload
(defun durand-view-last-day ()
  "Match the last day"
  (interactive)
  (durand-change-parameter :unit 'day))

;;;###autoload
(defun durand-view-last-week ()
  "Match the last week"
  (interactive)
  (durand-change-parameter :unit 'week))

;;;###autoload
(defun durand-view-last-month ()
  "Match the last month"
  (interactive)
  (durand-change-parameter :unit 'month))

;;;###autoload
(defun durand-view-last-year ()
  "Match the last year"
  (interactive)
  (durand-change-parameter :unit 'year))

;;;###autoload
(defun durand-view-last-custom ()
  "Match a custom time period"
  (interactive)
  (let (;; (beg (read-string "Le début: "))
        (beg (durand-account-normalize-time (org-read-date t t nil "Le début")))
        ;; (end (read-string "La fin: "))
        (end (durand-account-normalize-time (org-read-date t t nil "La fin"))))
    ;; (durand-change-parameter :unit (string-join (list beg end) ":"))
    (durand-change-parameter :unit (list beg end))))

;;;###autoload
(defun durand-view-include ()
  "Change sum-type"
  (interactive)
  (let ((st (read-string "Inclus: ")))
    (durand-change-parameter :sum-type st)))

;;;###autoload
(defun durand-view-exclude ()
  "Change exclude-type"
  (interactive)
  (let ((et (read-string "Exclus: ")))
    (durand-change-parameter :exclude-type et)))

;;;###autoload
(defun durand-view-report-mode ()
  "Change report-mode"
  (interactive)
  (durand-change-parameter :report-mode
                           (intern (completing-read "Mode: " '("separate" "combine")
                                                    nil t))))

;;;###autoload
(defvar durand-account-prev-position nil
  "The previous position to go to.")

;;;###autoload
(defun durand-view-entry ()
  "Make entry go to top or the previous position"
  (interactive)
  (let ((window-line (count-visible-lines (window-start) (point))))
    (cond
     ((and (= window-line (1+ scroll-margin))
           durand-account-prev-position)
      (recenter durand-account-prev-position))
     ((= window-line (1+ scroll-margin))
      (message "No previous position to go to")
      (setf durand-account-prev-position window-line))
     (t
      (setf durand-account-prev-position window-line)
      (recenter 0)))))

;; define a report mode for reporting

;;;###autoload
(define-derived-mode account-report-mode special-mode "Account Report"
  "A mode for reporting the account.
\\<account-report-mode-map>
Press \\[durand-view-last-day] to view the last day;
\\[durand-view-last-week] to view the last week;
\\[durand-view-last-month] to view the last month;
\\[durand-view-last-year] to view the last year;
\\[durand-view-last-custom] to specify a custom continuous range.")

;;;###autoload
(defun durand-account-first-entry ()
  "Go to the first entry in the report."
  (interactive)
  (goto-char (point-min))
  (durand-view-go-to-next-day 1))

;;;###autoload
(defun durand-account-last-entry ()
  "Go to the last entry in the report."
  (interactive)
  (goto-char (point-max))
  (durand-view-go-to-previous-day 1))

;;;###autoload
(defun durand-view-go-to-next-day (&optional arg)
  "Go to the next ARG day."
  (interactive "p")
  (forward-char)
  (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t arg)
  (beginning-of-line))

;;;###autoload
(defun durand-view-go-to-previous-day (&optional arg)
  "Go to the next ARG day."
  (interactive "p")
  (ignore-errors (forward-char))
  (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t (- arg))
  (beginning-of-line))

;;;###autoload
(defun durand-view-go-to-account-day ()
  "Go to the corresponding date.
If executed conitnuously, then scroll down the account buffer."
  (interactive)
  (unless (string= (buffer-name) "*ACCOUNT REPORT*")
    (user-error "This should only be executed in account report buffer."))
  (cond
   ((eq last-command this-command)
    (select-window (get-buffer-window "account.org"))
    (evil-scroll-line-down (/ (window-body-height) 2)))
   ((save-excursion
      (beginning-of-line)
      (looking-at "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"))
    (let ((str (match-string-no-properties 0)))
      (select-window (get-buffer-window "account.org"))
      (goto-char (org-find-pos-of-day str))
      (org-map-entries #'outline-show-entry nil 'tree)
      (recenter 0)))
   ((save-excursion
      (beginning-of-line)
      (looking-at "^  "))
    (while (not (looking-at "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"))
      (beginning-of-line 0))
    (let ((str (match-string-no-properties 0)))
      (select-window (get-buffer-window "account.org"))
      (goto-char (org-find-pos-of-day str))
      (org-map-entries #'outline-show-entry nil 'tree)
      (recenter 0)))
   (t
    (user-error "No date specified here!")))
  (select-window (get-buffer-window "*ACCOUNT REPORT*")))

;;;###autoload
(defun durand-agenda ()
  "My own customized agenda view."
  (interactive)
  (org-agenda nil "o")
  (org-agenda-first-block)
  (evil-emacs-state))

;; The original function does not offer an option to select all.
;;;###autoload
(defun org-offer-links-in-entry (buffer marker &optional nth zero)
  "Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it.

Modified by Durand"
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char marker)
     (let ((cnt ?0)
           have-zero end links link c)
       (when (and (stringp zero) (string-match org-link-bracket-re zero))
         (push (match-string 0 zero) links)
         (setq cnt (1- cnt) have-zero t))
       (save-excursion
         (org-back-to-heading t)
         (setq end (save-excursion (outline-next-heading) (point)))
         (while (re-search-forward org-link-any-re end t)
           (push (match-string 0) links))
         (setq links (org-uniquify (reverse links))))
       (cond
        ((null links)
         (message "No links"))
        ((and (integerp nth) (< nth 0))
         (setq link links))
        ((equal (length links) 1)
         (setq link (car links)))
        ((and (integerp nth) (>= nth 0) (>= (length links) (if have-zero (1+ nth) nth)))
         (setq link (nth (if have-zero nth (1- nth)) links)))
        (t                              ; we have to select a link
         (save-excursion
           (save-window-excursion
             (delete-other-windows)
             (with-output-to-temp-buffer "*Select Link*"
               (dolist (l links)
                 (cond
                  ((not (string-match org-link-bracket-re l))
                   (princ (format "[%c]  %s\n" (cl-incf cnt)
                                  (org-unbracket-string "<" ">" l))))
                  ((match-end 3)
                   (princ (format "[%c]  %s (%s)\n" (cl-incf cnt)
                                  (match-string 3 l) (match-string 1 l))))
                  (t (princ (format "[%c]  %s\n" (cl-incf cnt)
                                    (match-string 1 l)))))))
             (org-fit-window-to-buffer (get-buffer-window "*Select Link*"))
             (message "Select link to open, RET to open all:")
             (setq c (read-char-exclusive))
             (and (get-buffer "*Select Link*") (kill-buffer "*Select Link*"))))
         (when (equal c ?q) (user-error "Abort"))
         (if (equal c ?\C-m)
             (setq link links)
           (setq nth (- c ?0))
           (when have-zero (setq nth (1+ nth)))
           (unless (and (integerp nth) (>= (length links) nth))
             (user-error "Invalid link selection"))
           (setq link (nth (1- nth) links)))))
       (cons link end)))))

;;;###autoload
(defun org-super-agenda-next-group ()
  "Go to the next group in org super agenda buffer"
  (interactive)
  (if (save-match-data
        (save-excursion
          (beginning-of-line)
          (looking-at "^\\s-\\S-")))
      (progn
        (end-of-line)
        (re-search-forward "^\\s-\\S-" nil 'go))
    (re-search-forward "^\\s-\\S-" nil 'go))
  (when (= (point) (point-max))
    (org-agenda-next-block))
  (beginning-of-line)
  (recenter))

;;;###autoload
(defun org-agenda-move-item-advice (old arg)
  "Apply move-block functions if cannot move item anymore"
  (while (> arg 0)
    (let ((ori (point)))
      (apply old (list 1))
      (cl-decf arg)
      (and (= ori (point))
           (let ((cur-bloc (let ((num 0))
                             (dolist (p org-agenda-block-seps num)
                               (when (>= (point) p)
                                 (setf num (1+ num))))))
                 (total-bloc (length org-agenda-block-seps)))
             (cond
              ((eq this-command 'org-agenda-next-item)
               (if (= total-bloc cur-bloc)
                   (message "Le dernier ligne")
                 (widen)
                 (goto-char (nth cur-bloc org-agenda-block-seps))
                 (org-agenda-narrow-block)
                 (org-agenda-next-item 1)))
              ((eq this-command 'org-agenda-previous-item)
               (if (= cur-bloc 1)
                   (message "Le premier ligne")
                 (widen)
                 (goto-char (nth (- cur-bloc 2) org-agenda-block-seps))
                 (org-agenda-narrow-block)
                 (goto-char (point-max))))
              (t
               (user-error "This advice is used in weird places: %s" this-command))))))))

(advice-add 'org-agenda-next-item :around 'org-agenda-move-item-advice)
(advice-add 'org-agenda-previous-item :around 'org-agenda-move-item-advice)

;; The original function uses the point (1+ (point-at-eol)), which will throw an
;; error if that is bigger than (point-max), so fix it.
;;;###autoload
(defun org-remove-subtree-entries-from-agenda (&optional buf beg end)
  "Remove all lines in the agenda that correspond to a given subtree.
The subtree is the one in buffer BUF, starting at BEG and ending at END.
If this information is not given, the function uses the tree at point."
  (let ((buf (or buf (current-buffer))) m p)
    (save-excursion
      (unless (and beg end)
        (org-back-to-heading t)
        (setq beg (point))
        (org-end-of-subtree t)
        (setq end (point)))
      (set-buffer (get-buffer org-agenda-buffer-name))
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line 1)
        (while (not (bobp))
          (when (and (setq m (org-get-at-bol 'org-marker))
                     (equal buf (marker-buffer m))
                     (setq p (marker-position m))
                     (>= p beg)
                     (< p end))
            (let ((inhibit-read-only t))
              (delete-region (point-at-bol) (min (point-max) (1+ (point-at-eol))))))
          (beginning-of-line 0))))))

;;;###autoload
(cl-defun org-agenda-next-block (&optional (n 1))
  "Go to the next N-th block in org agenda buffer"
  (interactive)
  (let* ((cands (-filter (lambda (x)
                           (> x (point)))
                         org-agenda-block-seps))
         (res (if cands
                  (if (>= (length cands) n)
                      (nth (1- n) cands)
                    (org-agenda-last-block)
                    (user-error "Seulement %d blocs après, mais %d fois demandé" (length cands) n))
                (user-error "Le dernier bloc"))))
    (widen)
    (goto-char res)
    (org-agenda-narrow-block)))

;;;###autoload
(defvar org-agenda-total-blocks nil
  "Le nombre total de blocs dans org-agenda")

;;;###autoload
(defvar org-agenda-block-seps nil
  "Les séparateurs de blocs dans org-agenda comme une liste des positions")

(make-variable-buffer-local 'org-agenda-total-blocks)
(make-variable-buffer-local 'org-agenda-block-seps)

;;;###autoload
(defun org-agenda-count-blocks (&optional debut)
  "Count the total number of blocks in the agenda, starting from DEBUT, if non-nil"
  (save-restriction
    (save-excursion
      (save-match-data
        (widen)
        (goto-char (or debut (point-min)))
        (let* ((num 1)
               (res (list (point-min)))
               fin)
          (cl-loop
           for pos = (next-single-property-change (line-end-position) 'durand-agenda-regular-header)
           while pos
           do
           (setf num (1+ num)
                 res (append res (list pos)))
           (goto-char pos))
          (setf
           org-agenda-total-blocks num
           org-agenda-block-seps res))))))

;;;###autoload
(defun org-agenda-show-blocks-number ()
  "Show the current position"
  (let ((num (length (-filter (lambda (x) (>= (point) x)) org-agenda-block-seps))))
    (and num (format " %d/%d" num (if org-agenda-total-blocks
                                      org-agenda-total-blocks
                                    0)))))

;;;###autoload
(defun org-super-agenda-previous-group ()
  "Go to the next group in org super agenda buffer"
  (interactive)
  (if (save-match-data
        (save-excursion
          (beginning-of-line)
          (looking-at "^\\s-\\S-")))
      (progn
        (beginning-of-line)
        (re-search-forward "^\\s-\\S-" nil 'go -1))
    (re-search-forward "^\\s-\\S-" nil 'go -1))
  (when (= (point) (point-min))
    (org-agenda-previous-block)
    (goto-char (point-max)))
  (beginning-of-line)
  (recenter))

;;;###autoload
(cl-defun org-agenda-previous-block (&optional (n 1))
  "Go to the next block in org agenda buffer"
  (interactive)
  (let* ((cands (-filter (lambda (x)
                           (< x (point)))
                         org-agenda-block-seps))
         (res (if cands
                  (if (>= (length cands) n)
                      (nth (1- n) (nreverse cands))
                    (org-agenda-first-block)
                    (user-error "Seulement %d blocs avant, mais %d fois demandé" (length cands) n))
                (user-error "Le premier bloc"))))
    (widen)
    (goto-char res)
    (org-agenda-narrow-block)))

;;;###autoload
(defun org-agenda-narrow-block ()
  "Narrow to one block"
  (interactive)
  (widen)
  (let* ((end (or
               (let ((chois (next-single-property-change (line-end-position) 'durand-agenda-regular-header)))
                 (and chois
                      (1- chois)))
               (point-max)))
         (start (save-excursion
                  (goto-char
                   (or
                    (previous-single-property-change (line-end-position) 'durand-agenda-regular-header)
                    (point-min)))
                  (line-beginning-position))))
    (narrow-to-region start end)
    (goto-char (point-min))))

;; After an update, the property `org-agenda-structural-header' does not suffice
;; any more to distinguish a super agenda header from a regular agenda header,
;; per change by some /fix/ of the package. So I have to prepare the agenda
;; buffer by giving new text properties at the suitable places.

;;;###autoload
(defun durand-prepare-agenda ()
  "Prepare the agenda buffer to be used by custom functions."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (add-text-properties (line-beginning-position)
                           (line-end-position)
                           '(durand-agenda-regular-header t))
      (forward-line 1)
      (forward-char 1)
      (while (re-search-forward "^\\S-" nil t)
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             '(durand-agenda-regular-header t))))))

(add-hook! 'org-agenda-finalize-hook
           'durand-prepare-agenda
           'org-agenda-count-blocks
           'org-agenda-first-block)
;; (add-hook 'org-agenda-finalize-hook 'org-agenda-first-block)
;; (add-hook 'org-agenda-finalize-hook 'org-agenda-count-blocks)
;; (add-hook 'org-agenda-finalize-hook 'durand-prepare-agenda)
;; (remove-hook 'org-agenda-finalize-hook 'durand-agenda-mode)

;; (define-derived-mode durand-agenda-mode org-agenda-mode "Durand-Agenda"
;;   "My agenda mode")

;; fix org-agenda-kill
;;;###autoload
(defun org-agenda-kill ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  ;; use derived-mode-p instead of requiring strictly to be in org-agenda-mode
  (or (derived-mode-p 'org-agenda-mode) (error "Not in agenda"))
  (let* ((bufname-orig (buffer-name))
         (marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         (type (org-get-at-bol 'type))
         dbeg dend (n 0) conf)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (if (and (derived-mode-p 'org-mode) (not (member type '("sexp"))))
              (setq dbeg (progn (org-back-to-heading t) (point))
                    dend (org-end-of-subtree t t))
            (setq dbeg (point-at-bol)
                  dend (min (point-max) (1+ (point-at-eol)))))
          (goto-char dbeg)
          (while (re-search-forward "^[ \t]*\\S-" dend t) (setq n (1+ n)))))
      (setq conf (or (eq t org-agenda-confirm-kill)
                     (and (numberp org-agenda-confirm-kill)
                          (> n org-agenda-confirm-kill))))
      (and conf
           (not (y-or-n-p
                 (format "Delete entry with %d lines in buffer \"%s\"? "
                         n (buffer-name buffer))))
           (error "Abort"))
      (let ((org-agenda-buffer-name bufname-orig))
        (ignore org-agenda-buffer-name)
        (org-remove-subtree-entries-from-agenda buffer dbeg dend))
      (with-current-buffer buffer (delete-region dbeg dend))
      (message "Agenda item and source killed"))))

;; La fonction `org-agenda-set-mode-name' est carrément inutile!
;; La fonction originale est dans le fichier org-agenda.el
;;;###autoload
(defun org-agenda-set-mode-name ()
  "Cette fonction ne sert rien!")

;; redefine org-agenda-goto
;; (require 'org-agenda)
;;;###autoload
(defun org-agenda-goto (&optional highlight)
  "Go to the entry at point in the corresponding Org file."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (push-mark)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-show-context 'agenda)
      (recenter 1)			; The change with the default function
      (org-back-to-heading t)
      (let ((case-fold-search nil))
        (when (re-search-forward org-complex-heading-regexp nil t)
          (goto-char (match-beginning 4)))))
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

;; custom functionalities
;;;###autoload
(defun org-agenda-goto-with-fun (fun &optional _highlight)
  "Go to the entry at point in the corresponding Org file, and execute FUN."
  (interactive)
  (save-window-excursion
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (switch-to-buffer-other-window buffer)
      (widen)
      (push-mark)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
        (org-show-context 'agenda)
        (recenter 1)
        (org-back-to-heading t)
        (let ((case-fold-search nil))
          (when (re-search-forward org-complex-heading-regexp nil t)
            (goto-char (match-beginning 4)))))
      (funcall fun))))

;; It turns out I do not need these functions now, as I directly go to the account file.

;;;###autoload
;; (defun org-agenda-update-link (&optional arg)
;;   "Update the link in agenda buffer"
;;   (interactive "P")
;;   (if arg
;;       (org-agenda-goto-with-fun 'org-update-link)
;;     (org-agenda-goto-with-fun 'org-immediate-update-link)))

;; (defvar durand-before-obj nil
;;   "The position to return to when ivy-read ends")

;;;###autoload
;; (defun durand-cursor-follow-link ()
;;   "Place the cursor on the link in ivy-read; inspired by swiper"
;;   (with-ivy-window
;;     (swiper--cleanup)
;;     (let* ((cur (ivy-state-current ivy-last))
;;            (beg (and cur (cadr (assoc-default cur (caddr durand-before-obj)))))
;;            (end (and cur (caddr (assoc-default cur (caddr durand-before-obj)))))
;;            (wnd (ivy-state-window ivy-last)))
;;       (swiper--add-overlay beg end 'swiper-line-face wnd 0))))

;;;###autoload
(cl-defun org-update-link (&optional link choose-first choose-link desc)
  "Update the link in an organized way.
LINK, if non-`nil', is the link to replace the old one.

If CHOOSE-FIRST is non-`nil', then choose the first link in the
contained links to replace.

If CHOOSE-LINK is non-`nil', and LINK is `nil', then offer to
choose the link to replace the old one. Otherwise, use the link
in `org-store-link-props' or the first link in
`org-stored-links', in this order. If all the above methods fail,
then it errors out.

The description of the new link comes from DESC if non-`nil'; if
it is `nil', then it comes from the link to replace. If it does
not offer one, then it uses the current clipboard as the
description."
  (interactive)
  (let* ((next-heading-position (save-excursion
                                  (outline-next-heading)
                                  (point)))
         (contained-links (save-excursion
                            (nreverse
                             (cl-loop
                              while (re-search-forward org-link-any-re next-heading-position t)
                              collect
                              (list (match-string-no-properties 2)
                                    (match-string-no-properties 3)
                                    (match-beginning 0)
                                    (match-end 0))))))
         (choice-to-update (cond
                            ((null contained-links) nil)
                            (choose-first (car contained-links))
                            (t
                             (let* ((icomplete-compute-delay 0)
                                    (choice
                                     (completing-read
                                      "Chois un lien à remplaçer: "
                                      (mapcar (lambda (x)
                                                (concat (cadr x) " - " (car x)))
                                              contained-links))
                                     ;; (icomplete-vertical-do '(:height (/ (frame-height) 4))
                                     ;;   (completing-read
                                     ;;    "Chois un lien à remplaçer: "
                                     ;;    (mapcar (lambda (x)
                                     ;;              (concat (cadr x) " - " (car x)))
                                     ;;            contained-links)))
                                     ))
                               (ignore icomplete-compute-delay)
                               (cl-find choice contained-links
                                        :test (lambda (x y)
                                                (string= x (concat (cadr y) " - " (car y)))))))))
         (all-cands (append contained-links org-stored-links))
         (chosen-link (cond
                       (link (list link))
                       (choose-link
                        (cl-assoc (completing-read "Chois un lien pour remplaçer: "
                                                   all-cands)
                                  all-cands
                                  :test #'string=))
                       (org-store-link-plist
                        (list
                         (plist-get org-store-link-plist :link)
                         (plist-get org-store-link-plist :description)))
                       (org-stored-links
                        (car org-stored-links))
                       (t
                        (user-error "Je ne peux pas déterminer le lien automatiquement."))))
         (description (read-string "Description: "
                                   (cond
                                    ((and (stringp desc)
                                          (null (string= desc "")))
                                     desc)
                                    ((and (cadr chosen-link)
                                          (not (string= (cadr chosen-link) "")))
                                     (file-name-nondirectory (cadr chosen-link)))
                                    (chosen-link
                                     (car chosen-link))
                                    (t
                                     (current-kill 0 t)))))
         (nouveau-lien (org-make-link-string (car chosen-link) description))
         (update-or-insert
          ;; NOTE: `t' means update; `nil' means insert a new one.
          (if (cl-member choice-to-update contained-links :test #'equal) t nil)))
    (if update-or-insert
        (setf (buffer-substring (nth 2 choice-to-update)
                                (nth 3 choice-to-update))
              nouveau-lien)
      (org-end-of-meta-data)
      (newline)
      (newline)
      (forward-char -1)
      (insert nouveau-lien)
      (indent-according-to-mode))))

;; REVIEW: This is a poorly written function, left here as a bad example.
;;;###autoload
;; (defun org-update-link (&optional link arg)
;;   "Update the link"
;;   (interactive)
;;   (let* ((next-heading-position (save-excursion
;;                                   (outline-next-heading)
;;                                   (point)))
;;          (current (let (res)
;;                     (save-excursion
;;                       (while (re-search-forward org-link-any-re
;;                                                 next-heading-position
;;                                                 t)
;;                         (push (list (match-string 2)
;;                                     (match-string 3)
;;                                     (match-beginning 0)
;;                                     (match-end 0))
;;                               res)))
;;                     (nreverse res)))
;;          (num_link (when current
;;                      (cond
;;                       (arg (substring-no-properties
;;                             (caar current)))
;;                       (t
;;                        (setf durand-before-obj (list (point) (buffer-name) current))
;;                        (let ((choice (ivy-read "Chois un lien à remplacer?"
;;                                                (mapcar (lambda (x)
;;                                                          (concat (substring-no-properties
;;                                                                   (cadr x))
;;                                                                  " - "
;;                                                                  (substring-no-properties
;;                                                                   (car x))))
;;                                                        current)
;;                                                ;; :update-fn 'durand-cursor-follow-link
;;                                                :unwind (lambda ()
;;                                                          ;; (swiper--cleanup)
;;                                                          (setf durand-before-obj nil)))))
;;                          (car (cl-find choice current
;;                                        :test (lambda (x y)
;;                                                (string=
;;                                                 x
;;                                                 (concat
;;                                                  (substring-no-properties (cadr y))
;;                                                  " - "
;;                                                  (substring-no-properties (car y))))))))))))
;;          (lien-courant (when (not (string= num_link ""))
;;                          (assoc num_link (mapcar (lambda (x)
;;                                                    (cons (car x) (cddr x)))
;;                                                  current))))
;;          (beg (and lien-courant (nth 1 lien-courant)))
;;          (end (and lien-courant (nth 2 lien-courant)))
;;          (collection (append (mapcar #'car current)
;;                              (mapcar #'car org-stored-links)))
;;          (chosen-link (when (not arg) (or link (ivy-read "Link: " collection))))
;;          (associate-current (when arg (assoc chosen-link current)))
;;          (associate (when (not arg) (assoc chosen-link org-stored-links)))
;;          (objet (if arg
;;                     (car org-stored-links)
;;                   (or associate-current associate (list chosen-link ""))))
;;          (lien (or link (car objet)))
;;          (desc (read-string "Desctiption: " (let ((default (cadr objet)))
;;                                               (if (and default (not (string= default "")))
;;                                                   (file-name-nondirectory default)
;;                                                 (current-kill 0 t)))))
;;          (nouveau-lien (org-make-link-string lien desc)))
;;     (if lien-courant
;;         (setf (buffer-substring beg end)
;;               nouveau-lien)
;;       (save-excursion
;;         (outline-next-heading)
;;         (newline)
;;         (newline)
;;         (backward-char 2)
;;         (setf (buffer-substring (point) (1+ (point)))
;;               nouveau-lien)
;;         (indent-according-to-mode)))))

;; NOTE: this is not used anymore.
;;;###autoload
;; (defun org-immediate-update-link ()
;;   "Mettre à jour le lien immédiatement"
;;   (org-update-link nil t))

;;;###autoload
(defun org-append-text ()
  "Append text to the end of the entry before the next heading"
  (interactive)
  (outline-next-heading)
  (let ((beg (point))
        (text (read-string "Text: "))
        (fill-column 80))
    (insert (concat "\n" text "\n\n"))
    (indent-region beg (point))
    (fill-region beg (point))))

;;;###autoload
(defun durand-org-agenda-append-text ()
  "Append some text to the end of the entry at the point"
  (interactive)
  (org-agenda-goto-with-fun 'org-append-text))

;;;###autoload
(defun durand-org-get-notes ()
  "View the notes of the org-entry at point.
This returns a list of notes, where every element is a list whose `car' is the time,
and whose `caddr' is a list of strings, the content of the note."
  (interactive)
  (save-restriction
    (save-excursion
      (save-match-data
        (widen)
        (unless (looking-at org-heading-regexp)
          (outline-back-to-heading t))
        (let ((limit (save-excursion (outline-next-heading) (point)))
              res-list)
          (while (re-search-forward org-note-regexp limit t)
            (let ((indent-string (match-string 1))
                  (time-string (match-string 2))
                  (beg (1+ (point)))
                  res)
              (forward-line)
              (while (looking-at (concat "^" indent-string "\\s-\\{2\\}"))
                (push (buffer-substring-no-properties
                       (match-end 0)
                       (line-end-position))
                      res)
                (forward-line))
              (push (list
                     (substring-no-properties time-string)
                     (list beg (1- (point)))
                     (mapconcat #'identity (nreverse res) "\n"))
                    res-list)))
          (nreverse res-list))))))

(after! org
  (require 'org-element)
;;;###autoload
  (defvar org-note-regexp nil
    "The regexp for notes in a org heading.")
  (setq org-note-regexp (concat
			                   "^\\([ \t]*\\)- \\(?:Note taken on\\|CLOSING NOTE\\) \\("
			                   org-element--timestamp-regexp
			                   "\\).*$")))

;;;###autoload
(defun durand-org-get-logs ()
  "Get the logging information of a headline."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (unless (org-at-heading-p) (outline-back-to-heading))
        (let ((limit (save-excursion (outline-next-heading) (point))))
          (cond
           ((re-search-forward ":LOGBOOK:" limit t)
            (forward-line 1)
            (let ((ending (save-excursion (re-search-forward ":END:" limit t) (point))))
              (cl-loop
               while (re-search-forward
                      (concat "\\(?:State\\|CLOSING NOTE\\)" ".*" org-ts-regexp3)
                      ending t)
               collect (encode-time
                        0
                        (string-to-number (cond ((stringp (match-string 8))
                                                 (match-string 8))
                                                (t "0")))
                        (string-to-number (cond ((stringp (match-string 7))
                                                 (match-string 7))
                                                (t "0")))
                        (string-to-number (match-string 4))
                        (string-to-number (match-string 3))
                        (string-to-number (match-string 2))))))))))))

;;;###autoload
(defun durand-list-str-valued-plist-p (x)
  "Return t if X is a plist with values all lists of strings."
  (while (consp x)
    (setf x
          (cond
           ((not
             (and (keywordp (car x))
                  (consp (cdr x))))
            'not-plist)
           ((not (and (listp (cadr x))
                      (cl-every 'stringp
                                (cadr x))))
            'not-list-of-string-valued)
           (t
            (cddr x)))))
  (null x))

;;;###autoload
(defun durand-org-map-entries (func &optional match)
  "Call FUNC for each entry that matches MATCH.
MATCH is a plist whose following keys have special meanings.
:str => list of literal string search strings
:regexp => list of regular expressions
:tags => list of tags to search."
  (require 'org)
  (cl-assert (derived-mode-p 'org-mode))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
               when
               (save-excursion
                 (goto-char (match-beginning 0))
                 (let ((limit (save-excursion
                                (outline-next-heading)
                                (point)))
                       (tags (org-get-tags (point))))
                   (and (or (null (plist-get match :str))
                            (cl-some
                             (lambda (str)
                               (save-excursion (search-forward str limit t)))
                             (plist-get match :str)))
                        (or (null (plist-get match :regexp))
                            (cl-some
                             (lambda (re)
                               (save-excursion (re-search-forward re limit t)))
                             (plist-get match :regexp)))
                        (or (null (plist-get match :tags))
                            (cl-every
                             (lambda (tag)
                               (cl-member tag tags :test 'string=))
                             (plist-get match :tags))))))
               collect (save-excursion (funcall func))))))

;;;###autoload
(defun durand-org-view-notes ()
  "View the notes entries in a separate buffer"
  (interactive)
  (let* ((notes (durand-org-get-notes))
         (logs (durand-org-get-logs))
         (logs (cl-sort logs 'time-less-p))
         (len (length notes)))
    (with-current-buffer-window
        "*durand-org-view-notes*"
        nil nil
      (goto-char (point-min))
      (insert "#+STARTUP: showall\n")
      ;; insert log graph if any
      (ignore-errors
        (when (not (null logs))
          (insert "LOGS:\n")
          ;; (durand-draw-days logs)
          (durand-draw-calendar-days logs)))
      (insert "\n")
      ;; insert notes
      (let ((times (mapcar #'car notes))
            (metas (mapcar #'cadr notes))
            (contents (mapcar #'caddr notes)))
        (if (/= 0 len)
            (dotimes (i len)
              (insert (concat
                       "\n"
                       (propertize "*" :note-meta (elt metas i))
                       " Note on "
                       (elt times i)
                       "\n"
                       (elt contents i)
                       "\n")))
          (insert "No notes found!")))
      (goto-char (point-min))
      (durand-org-notes-mode)
      (when (fboundp 'writegood-mode)
        (writegood-mode -1))
      ;; (let ((temp-map (make-sparse-keymap)))
      ;;   (set-keymap-parent temp-map org-mode-map)
      ;;   (map! :map temp-map
      ;;         :n [?q] 'quit-window)
      ;;   (use-local-map temp-map))
      )
    (message "%s note%s found" (if (= 0 len) "No" (number-to-string len))
             (cond ((= len 0) "s") ((<= len 1) "") (t "s")))))

;;;###autoload
(defun durand-org-view-all-logs (&optional match file start-date end-date
                                           number-of-months-per-row)
  "View all logs recorded in FILE, including archived files matched by MATCH.
MATCH has the following possibilities:

- a string          => use regular expression to search.
- a list of strings => list of tags to search.
- a plist           =>
  + :STR    => list of literal search strings
  + :REGEXP => list of regular expressions to search
  + :TAGS   => list of tags to search

If FILE is nil or omitted, then it defaults to
\"aujourdhui.org\".

START-DATE and END-DATE, if non-nil, specify the start and the
end date of the search, respectively.

NUMBER-OF-MONTHS-PER-ROW if non-nil specifies the number of
months to draw on one line. It defaults
to (floor (+ (window-text-width) 2) 22)."
  (interactive)
  (let* ((match
          (cond
           ((null match) (list :tags (list "run")))
           ((stringp match) (list :regexp (list match)))
           ((and (listp match)
                 (cl-every 'stringp match))
            (list :tags match))
           ((durand-list-str-valued-plist-p match) match)
           (t (user-error
               "MATCH should be either nil, a string, a list of strings, or a plist, but got %s"
               match))))
         (log-buffer-name (format "*logs: %S" match " *"))
         (archive-number -1)
         (file (or file "/Users/durand/org/aujourdhui.org"))
         logs)
    (let ((archive-file
           (format (if (> archive-number -1)
                       (car (split-string org-archive-location "::"))
                     "%s")
                   (concat
                    (file-name-sans-extension file)
                    (when (> archive-number 0) (format "(%d)" archive-number))
                    "."
                    (file-name-extension file)))))
      (while (file-exists-p archive-file)
        ;; (message "%d, %s" archive-number archive-file)
        (with-current-file archive-file nil
          (setf logs (append logs
                             (apply 'append
                                    (durand-org-map-entries 'durand-org-get-logs match))))
          (when start-date
            (setf logs
                  (cl-remove-if (lambda (time-value)
                                  (not
                                   (time-less-p
                                    (durand-date-to-time start-date)
                                    time-value)))
                                logs)))
          (when end-date
            (setf logs
                  (cl-remove-if (lambda (time-value)
                                  (not
                                   (time-less-p
                                    time-value
                                    (durand-date-to-time end-date))))
                                logs))))
        (setf archive-number (+ archive-number 1)
              archive-file (format (if (> archive-number -1)
                                       (car (split-string org-archive-location "::"))
                                     "%s")
                                   (concat
                                    (file-name-sans-extension file)
                                    (when (> archive-number 0) (format "(%d)" archive-number))
                                    "."
                                    (file-name-extension file))))))
    (setf logs (cl-sort logs 'time-less-p))
    (unless (get-buffer log-buffer-name)
      (get-buffer-create log-buffer-name))
    (with-current-buffer log-buffer-name
      (erase-buffer)
      (durand-draw-calendar-days logs number-of-months-per-row)
      (durand-org-notes-mode)
      (when (fboundp 'writegood-mode)
        (writegood-mode -1)))
    (switch-to-buffer log-buffer-name)))

;; NOTE: This is a helper for `durand-draw-calendar-days'.
;;;###autoload
(defun durand-pop-all (list-of-lists)
  "Pop each list in LIST-OF-LISTS. Return the list of poped results."
  (cl-loop for tail on list-of-lists by 'cdr
           collect (pop (car tail))))

;;;###autoload
(defun durand-draw-calendar-days (days-list &optional number-of-months-per-row)
  "Draw days in calendar format.
DAYS-LIST should be a list of time values.
NUMBER-OF-MONTHS-PER-ROW is the number of months to draw in one line.
If it is nil, then it defaults to (floor (+ (window-text-width) 2) 22)."
  (let* ((start-day (car days-list))
         (months-list (list-months-between start-day (current-time)))
         ;; arg of `calendar-day-of-week' : (list month day year)
         (week-day (calendar-day-of-week (list (nth 4 (decode-time start-day))
                                               1
                                               (nth 5 (decode-time start-day)))))
         (week-header (mapconcat #'identity
                                 (list
                                  (propertize
                                   "Su"
                                   'font-lock-face 'calendar-weekend-header)
                                  (propertize
                                   "Mo"
                                   'font-lock-face 'calendar-weekday-header)
                                  (propertize
                                   "Tu"
                                   'font-lock-face 'calendar-weekday-header)
                                  (propertize
                                   "We"
                                   'font-lock-face 'calendar-weekday-header)
                                  (propertize
                                   "Th"
                                   'font-lock-face 'calendar-weekday-header)
                                  (propertize
                                   "Fr"
                                   'font-lock-face 'calendar-weekday-header)
                                  (propertize
                                   "Sa"
                                   'font-lock-face 'calendar-weekend-header))
                                 " "))
         (number-of-months-per-row (or number-of-months-per-row
                                       (floor (+ (window-text-width) 2) 22)))
         ;; res-objs format : a list of lists of the form (year month week-of-start-of-month days)
         res-objs res-strings)
    (while months-list
      (let* ((current-value (decode-time (pop months-list))) ;; avoid poping more than once
             (current-year (nth 5 current-value))
             (current-month (nth 4 current-value)))
        ;; push to the beginning
        (push (list current-year current-month week-day) res-objs)
        (setf week-day (mod (+ week-day (month-length current-month current-year)) 7))))
    (setf res-objs (nreverse res-objs))
    (dolist (day days-list)
      (let ((month-diff (-
                         (nth 4 (decode-time day))
                         (nth 4 (decode-time start-day))))
            (year-diff (-
                        (nth 5 (decode-time day))
                        (nth 5 (decode-time start-day)))))
        (setf (nth (+ (* year-diff 12) month-diff) res-objs)
              (append (nth (+ (* year-diff 12) month-diff) res-objs)
                      (list (nth 3 (decode-time day)))))))
    (dolist (obj res-objs)
      (let* ((header (propertize
                      (concat (calendar-month-name (nth 1 obj))
                              " " (number-to-string (nth 0 obj)))
                      'font-lock-face 'calendar-month-header))
             (padded-header (pad-string-to
                             (pad-string-to header (- 20 (/ (- 20 (length header)) 2)) 32)
                             20 32 t))
             (pointer 1)
             (special (cdddr obj))
             rows temp)
        (while (<= pointer (month-length (nth 1 obj) (nth 0 obj)))
          (setf temp nil)
          (let* ((len (cond
                       ((= pointer 1)
                        (- 7 (nth 2 obj)))
                       ((<= pointer (- (month-length (nth 1 obj) (nth 0 obj)) 7))
                        7)
                       (t
                        (1+ (- (month-length (nth 1 obj) (nth 0 obj)) pointer)))))
                 (str (if (= pointer 1)
                          (mapconcat #'identity
                                     (append (make-list (nth 2 obj) "  ")
                                             (dotimes (i len (progn (ignore i) (nreverse temp)))
                                               (let* ((day-num (+ i pointer))
                                                      (day-str
                                                       (if (member day-num special)
                                                           (propertize
                                                            (number-to-string day-num)
                                                            'font-lock-face 'warning)
                                                         (number-to-string day-num))))
                                                 (push (pad-string-to day-str 2 32)
                                                       temp))))
                                     " ")
                        (mapconcat #'identity
                                   (append
                                    (dotimes (i len (progn (ignore i) (nreverse temp)))
                                      (let* ((day-num (+ i pointer))
                                             (day-str
                                              (if (member day-num special)
                                                  (propertize
                                                   (number-to-string day-num)
                                                   'font-lock-face 'warning)
                                                (number-to-string day-num))))
                                        (push (pad-string-to day-str 2 32)
                                              temp)))
                                    (make-list (- 7 len) "  "))
                                   " "))))
            (push str rows)
            (cl-incf pointer len)))
        (push (append (list padded-header week-header) (nreverse rows))
              res-strings)))
    (setf res-strings (nreverse res-strings))
    (while res-strings
      (let* ((row (-take number-of-months-per-row res-strings))
             (max-num (apply 'max (mapcar 'length row)))
             row-strs)
        (setf res-strings
              (nthcdr number-of-months-per-row res-strings)
              row
              (cl-loop for element in row
                       collect
                       (append element
                               (make-list (- max-num (length element))
                                          (make-string 20 32))))
              row-strs
              (cl-loop while (cl-some 'consp row)
                       collect (string-join (durand-pop-all row) "  ")))
        (cl-loop for str in row-strs
                 do (progn (insert str) (newline)))
        (newline)))))

;;;###autoload
(defun month-length (month year)
  "Return the length of the month.
Since this is not independent of the year, it does not suffice to
give one integer as the month, which should be between 1 and 12."
  (pcase month
    ((pred (lambda (month) (member month '(1 3 5 7 8 10 12))))
     31)
    ((pred (lambda (month) (member month '(4 6 9 11))))
     30)
    (2
     (if (date-leap-year-p year) 29 28))
    (_
     (user-error "Wrong number as month: %d" month))))

;;;###autoload
(defun durand-draw-days (days-list)
  "Draw the days in a pretty way.
DAYS-LIST should be a list of time values."
  (let* ((starting-day (car days-list))
         (day-string (apply #'concat (durand-prepare-strings starting-day 'day)))
         (month-string (apply #'concat (durand-prepare-strings starting-day 'month)))
         (year-string (apply #'concat (durand-prepare-strings starting-day 'year)))
         (check-string (apply #'concat (durand-prepare-strings starting-day 'check)))
         (splitted-string-day (split-when-at-end-of-line (concat day-string "|")))
         (splitted-string-month (split-when-at-end-of-line (concat month-string "|")))
         (splitted-string-year (split-when-at-end-of-line (concat year-string "|")))
         (splitted-check-string
          (progn
            (mapc
             (lambda (day-n)
               (setf (substring check-string
                                (+ (* 5 (durand-dates-subtract day-n starting-day)) 2)
                                (+ (* 5 (durand-dates-subtract day-n starting-day)) 4))
                     "**"))
             days-list)
            (split-when-at-end-of-line (concat check-string "|")))))
    (cl-mapcar (lambda (alpha beta gamma delta)
                 (insert (propertize (make-string (window-body-width) ?\-)
                                     'font-lock-face '(:foreground "red" :background "gray10")))
                 (insert (propertize alpha
                                     'font-lock-face '(:foreground "red" :background "gray10")))
                 (insert "\n")
                 (insert (propertize beta
                                     'font-lock-face '(:foreground "red" :background "gray10")))
                 (insert "\n")
                 (insert (propertize gamma
                                     'font-lock-face '(:foreground "red" :background "gray10")))
                 (insert "\n")
                 ;; (insert (propertize delta
                 ;;                     'font-lock-face '(:foreground "red" :background "gray10")))
                 (insert delta)
                 (insert "\n"))
               splitted-string-year
               splitted-string-month
               splitted-string-day
               splitted-check-string)))

;;;###autoload
(defun split-when-at-end-of-line (str)
  "Draw the string but go down N lines when it exceeds the window.
Special attention is paid to strings like \vert and \ast.
Also give colors to \vert and \ast differently."
  (let* ((str (progn
                (while (string-match "\\\\vert[{}]*" str)
                  (setf str (replace-match "|" nil nil str)))
                (while (string-match "\\\\ast[{}]*" str)
                  (setf str (replace-match "*" nil nil str)))
                str))
         (str-list (string-to-list str))
         (cur-col (current-column))
         (wbw (window-body-width))
         (pointer (pop str-list))
         temp res)
    (while str-list
      (cond
       ((= (+ cur-col (length temp)) wbw)
        (push (concat (nreverse temp)) res)
        (setf cur-col 0
              temp nil))
       (t
        (push pointer temp)
        (setf pointer (pop str-list)))))
    (push pointer temp)
    (push (concat (nreverse temp)) res)
    (nreverse (mapcar (lambda (x)
                        (while (string-match "|" x)
                          (setf x (replace-match
                                   (propertize "\\\\vert"
                                               'font-lock-face
                                               '(:foreground "red"
                                                             :background "gray10"))
                                   nil nil x)))
                        (while (string-match "\\*" x)
                          (setf x (replace-match
                                   (propertize "\\\\ast"
                                               'font-lock-face
                                               '(:foreground "gold"
                                                             :background "gray10"))
                                   nil nil x)))
                        x)
                      res))))

;;;###autoload
(defun durand-prepare-strings (starting-date type)
  "Prepare header strings starting from date STARTING-DATE."
  (let* ((sy (nth 5 (decode-time starting-date)))
         (sm (nth 4 (decode-time starting-date)))
         (sd (nth 3 (decode-time starting-date)))
         (ct (current-time))
         (cy (nth 5 (decode-time ct)))
         (cm (nth 4 (decode-time ct)))
         (cd (nth 3 (decode-time ct)))
         (length-function
          (lambda (date type)
            "DATE might indicate the number of years or months, depending upon the TYPE."
            (let ((dy (nth 5 (decode-time date)))
                  (dm (nth 4 (decode-time date)))
                  (dd (nth 3 (decode-time date))))
              (ignore dd)
              (pcase type
                ('year
                 (cond
                  ((= dy sy)
                   (1+ (min (durand-dates-subtract
                             (encode-time 0 0 0 0 1 (1+ sy))
                             starting-date)
                            (durand-dates-subtract ct starting-date))))
                  (t
                   (1+ (min (durand-dates-subtract
                             (encode-time 0 0 0 0 1 (1+ dy))
                             (encode-time 0 0 0 1 1 dy))
                            (durand-dates-subtract
                             ct
                             (encode-time 0 0 0 1 1 dy)))))))
                ('month
                 (cond
                  ((and (= dm sm) (= dy sy))
                   (1+ (min (durand-dates-subtract
                             (encode-time 0 0 0 0 (1+ sm) sy)
                             starting-date)
                            (durand-dates-subtract ct starting-date))))
                  (t
                   (1+ (min (durand-dates-subtract
                             (encode-time 0 0 0 0 (1+ dm) dy)
                             (encode-time 0 0 0 1 dm dy))
                            (durand-dates-subtract
                             ct
                             (encode-time 0 0 0 1 dm dy)))))))))))
         (orig (pcase type
                 ('year
                  (mapcar
                   (lambda (this-year)
                     (let* ((year-string (number-to-string this-year))
                            ;; e.g. 2019
                            (total-length (length year-string))
                            ;; 4
                            (half-length (/ total-length 2))
                            (separator "|")
                            (post-padding (make-string (- (/ (1- (* (funcall
                                                                     length-function
                                                                     (encode-time 0 0 0 1 1 this-year)
                                                                     'year)
                                                                    5))
                                                             2)
                                                          half-length)
                                                       32))
                            (pre-padding (make-string (- (ceiling
                                                          (1- (* (funcall
                                                                  length-function
                                                                  (encode-time 0 0 0 1 1 this-year)
                                                                  'year)
                                                                 5))
                                                          2)
                                                         half-length)
                                                      32)))
                       (concat separator pre-padding year-string post-padding)))
                   (number-sequence sy cy)))
                 ('month
                  (mapcar
                   (lambda (this-month)
                     (let* ((month-number (nth 4 (decode-time this-month)))
                            (this-year (nth 5 (decode-time this-month)))
                            (month-string (pad-string-to
                                           (number-to-string month-number)
                                           2))
                            (separator "|")
                            (post-padding (make-string (- (/ (1- (* (funcall
                                                                     length-function
                                                                     (encode-time 0 0 0 1 month-number this-year)
                                                                     'month)
                                                                    5))
                                                             2)
                                                          1)
                                                       32))
                            (pre-padding (make-string
                                          (- (ceiling
                                              (1- (* (funcall
                                                      length-function
                                                      (encode-time 0 0 0 1 month-number this-year)
                                                      'month)
                                                     5))
                                              2)
                                             1)
                                          32)))
                       (concat separator pre-padding month-string post-padding)))
                   (list-months-between starting-date ct)))
                 ('day
                  (mapcar
                   (lambda (this-day)
                     (let* ((day-number (nth 3 (decode-time this-day)))
                            ;; (this-month (nth 4 (decode-time this-day)))
                            ;; (this-year (nth 5 (decode-time this-day)))
                            (day-string (pad-string-to
                                         (number-to-string day-number)
                                         2))
                            (separator "|")
                            (pre-padding (make-string 1 32))
                            (post-padding (make-string 1 32)))
                       (concat separator pre-padding day-string post-padding)))
                   (list-days-between starting-date ct)))
                 ('check
                  (mapcar
                   (lambda (this-day)
                     (let* ((day-number (nth 3 (decode-time this-day)))
                            (day-string "  ")
                            (separator "|")
                            (pre-padding (make-string 1 32))
                            (post-padding (make-string 1 32)))
                       (ignore day-number)
                       (concat separator pre-padding day-string post-padding)))
                   (list-days-between starting-date ct))))))
    (ignore cd cm day-number dd sd)
    orig))

;;;###autoload
(defun list-months-between (date1 date2)
  (let* ((ey (nth 5 (decode-time date2)))
         (em (nth 4 (decode-time date2)))
         (sy (nth 5 (decode-time date1)))
         (sm (nth 4 (decode-time date1)))
         (pointer date1)
         (res (list pointer)))
    (when (or (< sy ey)
              (and (= sy ey)
                   (<= sm em)))
      ;; lexicographic ordering
      (while (not (and (= (nth 5 (decode-time pointer))
                          ey)
                       (= (nth 4 (decode-time pointer))
                          em)))
        (setf pointer (encode-time 0 0 0
                                   1
                                   (1+ (nth 4 (decode-time pointer)))
                                   (nth 5 (decode-time pointer))))
        (push pointer res))
      (nreverse res))))

;;;###autoload
(defun list-days-between (date1 date2)
  (let* ((ey (nth 5 (decode-time date2)))
         (em (nth 4 (decode-time date2)))
         (ed (nth 3 (decode-time date2)))
         (sy (nth 5 (decode-time date1)))
         (sm (nth 4 (decode-time date1)))
         (sd (nth 3 (decode-time date1)))
         (pointer date1)
         (res (list pointer)))
    (when (not (eq (lexicographically-less (list sy sm sd) (list ey em ed)) 'greater))
      (while (not (eq (lexicographically-less
                       (list (nth 5 (decode-time pointer))
                             (nth 4 (decode-time pointer))
                             (nth 3 (decode-time pointer)))
                       (list ey em ed))
                      'same))
        (setf pointer (encode-time 0 0 0
                                   (1+ (nth 3 (decode-time pointer)))
                                   (nth 4 (decode-time pointer))
                                   (nth 5 (decode-time pointer))))
        (push pointer res))
      (nreverse res))))

;;;###autoload
(defun lexicographically-less (list1 list2)
  "Compare lexicographically.
The two lists should have the same lengths."
  (let ((continue t)
        (ans 'same))
    (while (and continue list1 list2)
      (let ((e1 (pop list1))
            (e2 (pop list2)))
        (cond
         ((< e1 e2)
          (setf ans 'less
                continue nil))
         ((> e1 e2)
          (setf ans 'greater
                continue nil)))))
    ans))

;;;###autoload
(defun durand-dates-subtract (&rest times)
  "Subtract times with days as units"
  (apply #'- (mapcar #'time-to-days times)))

;;;###autoload
(defun durand-merge-two-lists (a b)
  "Merge two lists whose elements are strings"
  (let* ((la (length a))
         (lb (length b))
         (maxl (max la lb)))
    (cl-loop for i from 1 to maxl
             collect
             (concat
              (or (nth (1- i) a) "")
              (or (nth (1- i) b) "")))))

;;;###autoload
(defun durand-org-agenda-goto-view-note ()
  "Go to the corresponding file and view the notes from the agenda file."
  (interactive)
  (org-agenda-goto-with-fun 'durand-org-view-notes)
  (temp-buffer-window-show "*durand-org-view-notes*")
  ;; (save-selected-window
  ;;   (pop-to-buffer "*durand-org-view-notes*")
  ;;   (goto-char (point-min))
  ;;   (fit-window-to-buffer nil temp-buffer-max-height))
  )

;; Go to the first block in block agenda view
;;;###autoload
(defun org-agenda-first-block ()
  "Go to the first block in block agenda view"
  (interactive)
  (widen)
  (goto-char (point-min))
  (org-agenda-narrow-block))

;;;###autoload
(defun org-agenda-go-to-block ()
  "Aller à un certain bloc"
  (interactive)
  (let* ((num (read-number "Quel bloc?" 1))
         (total (length org-agenda-block-seps)))
    (if (not (and (<= num total)
                  (>= num 1)))
        (message "Il n'y a que %d blocs." total)
      (widen)
      (goto-char (nth (1- num) org-agenda-block-seps))
      (org-agenda-narrow-block))))

;;;###autoload
(defun org-agenda-last-block ()
  "Go to the last block in block agenda view"
  (interactive)
  (widen)
  (if (and org-agenda-block-seps
           (consp org-agenda-block-seps))
      (goto-char (-last #'identity org-agenda-block-seps))
    (goto-char (point-max)))
  (org-agenda-narrow-block))

;; Modified tag group re to work in strings as well
(defvar durand-org-tag-group-re " +\\(:\\([[:alnum:]_@#%:]+\\):\\) *$"
  "Modified tag group re to work in strings as well")

;; Helper function that gets rid of todos, tags, and properties
;;;###autoload
(defun org-agenda-filter-extras (str)
  "Helper function that gets rid of todos, tags, and properties"
  (let* ((answer (substring-no-properties str))
         (case-fold-search nil)
         (org-todo-regexp "\\(ALMOST\\|DONE\\|HARD\\(?:-WORKING\\)?\\|IMPOSSIBLE\\|PENDING\\|S\\(?:OLVED\\|TART\\)\\|TO\\(?:-THINK\\|DO\\)\\|WORKING\\)")
         (remove-list `(,org-todo-regexp
                        ,durand-org-tag-group-re
                        ,org-priority-regexp
                        "^ +"
                        " +$")))
    (dolist (re remove-list answer)
      (when (string-match re answer)
        (setf answer (replace-match "" nil nil answer))))))

;; Collect all agenda items and jump to the selected one
;;;###autoload
(defun org-agenda-jump-to-item (&optional initial-input)
  "Collect all agenda items and jump to the selected one"
  (interactive)
  (let (items choice)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (next-single-property-change (point-at-eol) 'org-marker)
          (let ((item-pos (next-single-property-change (point-at-eol) 'org-marker)))
            (goto-char item-pos)
            (push (cons
                   (org-agenda-filter-extras (get-text-property (point) 'txt))
                   item-pos)
                  items)))
        (when items
          (setf items (nreverse items))
          (setf choice (completing-read "Jump to:" (mapcar #'car items)
                                        nil t initial-input)))))
    (when choice
      (goto-char (assoc-default choice items
                                (lambda (s r)
                                  (string-match (regexp-quote r) s)))))))

;;;###autoload
(defun org-agenda-open-novels ()
  "Open all novels; place the cursor at the first novel line."
  (interactive)
  (beginning-of-line)
  (while (get-text-property (point-at-bol) 'org-marker)
    (org-agenda-open-link -1)
    (forward-line 1)))

;;;###autoload
(defun org-open-bookmarks ()
  "Choose bookmarks to open."
  (interactive)
  (let* ((cands (with-current-file "/Users/durand/org/notes.org" nil
                  (org-map-entries #'durand-org-link-info "bookmarks")))
         (choice (let ((icomplete-compute-delay 0))
                   (ignore icomplete-compute-delay)
                   (durand-choose-list cands nil "Chois un lien: ")
                   ;; (icomplete-vertical-do '(:height (/ (frame-height) 4))
                   ;;                        (durand-choose-list cands nil "Chois un lien: "))
                   )))
    (mapc
     (lambda (x)
       (mapc #'durand-org-open-link
             (assoc-default x cands #'string-match)))
     choice)
    (assoc-default (car choice) cands #'string-match)))

;;;###autoload
(defun durand-org-link-info (&optional arg)
  "Return a cons of the headline text and the links contained therein.
If ARG is t, then return (text . point).
If ARG is `youtube' or `all', then return (text list point)"
  (let* ((pt (point))
         (texte (nth 4 (org-heading-components)))
         (limite (save-excursion (outline-next-heading) (point)))
         (liste
          (cl-loop while (re-search-forward org-link-any-re limite t)
                   when (and (match-string-no-properties 2)
                             (match-string-no-properties 3))
                   collect (list
                            (match-string-no-properties 2)
                            (match-string-no-properties 3)))))
    (cond
     ((null arg)
      (cons texte liste))
     ((memq arg '(youtube all))
      (list texte liste pt))
     (t
      (cons texte pt)))))

;;; Some specific faces for this function

;;;###autoload
(defface durand-result-arrow-face
  '((t
     (:inherit minibuffer-prompt :foreground "skyblue" :height 250)))
  "Face for the arrow on the result items used by `durand-ivy-format-function-arrow'.")

;;;###autoload
(defun durand-choose-list-format-function (cands matched)
  "Format candidates to `durand-choose-list'."
  (ivy--format-function-generic
   (lambda (e)
     (concat (propertize
              (format "%s "
                      (all-the-icons-material
                       "school"
                       :face (cond ((cl-member e matched :test #'string=)
                                    'durand-result-arrow-face)
                                   (t 'durand-arrow-face)))))
             (ivy--add-face e 'ivy-current-match)))
   (lambda (e)
     (if (cl-member e matched :test #'string=)
         (concat (propertize
                  (format "%s "
                          (all-the-icons-material "school" :face 'durand-result-arrow-face)))
                 e)
       (concat "   " e)))
   cands "\n"))

;;;###autoload
(defvar durand-choose-list-result nil
  "The variable to hold the result of `durand-choose-list'.
See the documentation of the function for more details.")

;;;###autoload
(defvar durand-choose-list-det nil
  "This variable controls whether `durand-choose-list' determines the candidates.")

;;;###autoload
(defvar durand-choose-list-exc nil
  "This variable controls whether `durand-choose-list' wants to exclude some candidate.")

;;;###autoload
(defun durand-choose-list (cands &optional all texte non-quick display-cadr no-require-match
                                 no-sort keep-cache-result keep-text)
  "Choose from an alist. Multiple selection is supported.
NO-SORT has no effect here: it is here to conform with the
arg-list of another functions with the same name, that I defined
when I am not using ivy.
If ALL is non-nil, add a choice to select all of them.
If NON-QUICK is non-nil, then offer the selection even when there
is only one candidate.
If DISPLAY-CADR is non-nil, then display cadr rather than car.
If NO-REQUIRE-MATCH is t, then don't require the selection to match.
If KEEP-CACHE-RESULT is non-nil, then don't set the result
variable to nil in the beginning.
If KEEP-TEXT is non-nil, then keep the text the user entered."
  (ignore no-sort)
  (if (and (= (length cands) 1) (null non-quick))
      (list (caar cands))
    (let ((cands (if all (cons '("all") cands) cands))
          (question (or texte "Chois un: ")))
      (setf durand-choose-list-result (cond
                                       ((null keep-cache-result) nil)
                                       (t durand-choose-list-result))
            durand-choose-list-det nil
            durand-choose-list-exc nil)
      (setf ivy--index 0
            cands (mapcar (lambda (x)
                            (cond
                             ((null display-cadr)
                              (cons (car x) x))
                             ((and (listp x) (> (length x) 1))
                              (cons (cadr x) x))
                             ((listp x)
                              (cons (car x) x))
                             (t
                              (user-error "durand-choose-list: argument not a list: %S" x))))
                          cands)
            ivy-text nil)
      (cl-loop
       with durand-choose-list-exc
       with durand-choose-list-det
       with ivy-format-functions-alist =
       '((durand-choose-list . (lambda (cands)
                                 (durand-choose-list-format-function cands durand-choose-list-result))))
       while (null durand-choose-list-det)
       for element = (ivy-read question cands
                               :require-match (not no-require-match)
                               :action '(1
                                         ("o" (lambda (x)
                                                (setf durand-choose-list-det t
                                                      durand-choose-list-exc nil))
                                          "default")
                                         ("m" (lambda (x)
                                                (setf durand-choose-list-det nil
                                                      durand-choose-list-exc nil))
                                          "continue")
                                         ("u" (lambda (x)
                                                (setf durand-choose-list-det nil
                                                      durand-choose-list-exc 'unmark))
                                          "unmark")
                                         ("e" (lambda (x)
                                                (setf durand-choose-list-det nil
                                                      durand-choose-list-exc t
                                                      ivy--index (max 0 (1- ivy--index))))
                                          "exclude"))
                               :preselect ivy--index
                               :initial-input (when keep-text ivy-text)
                               :caller 'durand-choose-list)
       do
       (ignore ivy-format-functions-alist)
       (cond ((eq durand-choose-list-exc 'unmark)
              (setf durand-choose-list-result
                    (cl-remove element durand-choose-list-result
                               :test 'string=)))
             (durand-choose-list-exc
              (setf durand-choose-list-result
                    (cl-remove element durand-choose-list-result
                               :test 'string=)
                    cands
                    (cl-remove
                     element cands
                     :test (lambda (x y)
                             (string= x (if (listp y) (car y) y))))))
             (t (setf durand-choose-list-result
                      (cons element durand-choose-list-result)
                      cands
                      (cl-remove-duplicates
                       (cons (list element element) cands)
                       :test 'string=
                       :key (lambda (x) (if (listp x) (car x) x)))))))
      (when (member "all" durand-choose-list-result) (setf durand-choose-list-result (mapcar #'car (cdr cands))))
      (setf durand-choose-list-result (cl-remove-duplicates durand-choose-list-result :test #'string=)
            durand-choose-list-result
            (mapcar
             (lambda (x)
               (cond ((assoc x cands #'string=)
                      (cadr (assoc x cands #'string=)))
                     (t x)))
             durand-choose-list-result))
      durand-choose-list-result)))

;;;###autoload
(defun durand-browse-url (url)
  "Browse URL with the browser `durand-browser'."
  (make-process
   :name "durand browser"
   :buffer nil
   :command (list "open" "-a" durand-browser url)))

;;;###autoload
(defun org-open-novels (&optional arg)
  "Choose novel to open.
Choose first an action to perform:
open: choose links that are not qidian pages to open.
update: update the novel links.
open qidian: only open qidian pages.
open all: offer every link to open."
  (interactive)
  (let* ((action (if (memq arg '(open update all qidian))
                     arg
                   (completing-read "Quel action: "
                                    '("open" "update" "open qidian" "open all")
                                    nil t)))
         (action (if (memq arg '(open update qidian all))
                     arg
                   (pcase action
                     ("open" 'open)
                     ("update" 'update)
                     ("open qidian" 'qidian)
                     ("open all" 'all)
                     (_ (user-error "Wrong choice: %s" action))))))
    (cond
     ((eq action 'open)
      (let* (cands)
        (with-current-file "/Users/durand/org/notes.org" nil
          (setf cands (org-map-entries #'durand-org-link-info "roman-ARCHIVE")))
        (let ((liste-de-choix
               (let ((sel (durand-choose-list cands t "Chois un roman: "))
                     temp)
                 (dolist (ele sel temp)
                   (setf temp (append temp
                                      (durand-choose-list
                                       (cl-remove-if
                                        (lambda (element)
                                          (string-match "qidian" (car element)))
                                        (assoc-default ele cands))
                                       t "Chois un lien: " nil t)))))))
          (mapc #'durand-browse-url liste-de-choix))))
     ((eq action 'update)
      (org-update-novels))
     ((eq action 'all)
      (let* (cands)
        (with-current-file "/Users/durand/org/notes.org" nil
          (setf cands (org-map-entries #'durand-org-link-info "roman-ARCHIVE")))
        (let ((liste-de-choix
               (let ((sel (durand-choose-list cands t "Chois un roman: "))
                     temp)
                 (dolist (ele sel temp)
                   (setf temp (append temp
                                      (durand-choose-list
                                       (assoc-default ele cands)
                                       t "Chois un lien: " nil t)))))))
          (mapc #'durand-browse-url liste-de-choix))))
     ((eq action 'qidian)
      (message "Opening novels' qidian pages...")
      (let* (cands)
        (with-current-file "/Users/durand/org/notes.org" nil
          (setf cands (org-map-entries #'durand-org-link-info "roman-ARCHIVE")))
        (let* ((liste-de-choix
                (durand-choose-list
                 cands
                 t "Chois un qidian page d'un roman: "))
               (choix-de-liens
                (cl-loop for roman in liste-de-choix
                         for element = (-filter
                                        (lambda (x)
                                          (string-match "^qidian" (cadr x)))
                                        (assoc-default roman cands #'string=))
                         append (car element))))
          (mapc #'durand-browse-url (-filter #'stringp choix-de-liens))))
      (message "Opening novels' qidian pages...DONE")))))

;;;###autoload
(defun org-update-novels (&optional desc)
  "Update the html link to a novel, or to a web_link.
If DESC is non-`nil', then it is the description of the new link."
  (interactive)
;;; HACK: Refocus the selected frame.
;;; I was doing this in the applescript. But for some reason it is messed up. So
;;; I let emacs gain focus by itself now.
  (select-frame-set-input-focus (selected-frame))
  (let* ((tags (completing-read "tag: " '("roman-ARCHIVE"
                                          "web_link-ARCHIVE")
                                nil t))
         (roman-p (string-match "roman" tags))
         (files '("/Users/durand/org/notes.org" "/Users/durand/org/math_article_links.org"))
         (prompt (if roman-p
                     "Chois un roman à mettre à jour: "
                   "Chois un web lien à mettre à jour: "))
         cands)
    (setf cands
          (cl-loop for file in files
                   append (with-current-file file nil
                            (org-map-entries
                             (lambda ()
                               (let ((orig (durand-org-link-info t)))
                                 (list (car orig) (cdr orig) file)))
                             tags))))
    (unless roman-p (setf cands (nreverse cands)))
    (let* ((choix (completing-read prompt cands nil t))
           (item (cl-assoc choix cands :test #'string=))
           (lien (read-string "Le lien: " (current-kill 0 t))))
      (with-current-file (caddr item) nil
        (goto-char (cadr item))
        (org-update-link lien nil nil desc)))))

;;;###autoload
(defun org-open-youtube ()
  "Choose youtube link to open.
With \\[universal-argument], just kill the entry.
With \\[universal-argument] \\[universal-argument], don't kill the entry."
  (interactive)
  (let (cands)
    (with-current-file "/Users/durand/org/youtube_links.org" nil
      (setf cands (org-map-entries
                   (lambda () (durand-org-link-info 'youtube))
                   "youtube-ARCHIVE")))
    (let* ((sel
            (let ((icomplete-compute-delay 0))
              (ignore icomplete-compute-delay)
              (durand-choose-list
               cands t "Chois une vidéo pour l'action:" t)))
           (action (completing-read "Quoi faire?"
                                    '("voir et supprimer"
                                      "voir sans supprimer"
                                      "juste inspecter"
                                      "supprimer seulement")
                                    nil t))
           (to-view (string-match "voir" action))
           (to-kill (and (string-match "supprimer" action)
                         (not (string-match "sans" action))))
           (to-inspect (string-match "inspecter" action))
           (download-or-not (when to-view (y-or-n-p "Download or not?"))))
      (setf sel (sort sel (lambda (x y)
                            (< (cadr (assoc-default x cands))
                               (cadr (assoc-default y cands))))))
      (with-current-file "/Users/durand/org/youtube_links.org" nil
        (dolist (x (reverse sel))
          (goto-char (cadr (assoc-default x cands)))
          (when to-kill (org-cut-subtree))
          (when to-view
            (dolist (y (car (assoc-default x cands)))
              (if download-or-not
                  (durand-download-youtube
                   (car y)
                   (concat x (durand-take-param (car y))))
                (browse-url (car y)))))))
      (when to-inspect
        (message "Les liens séléctionnés: %s" (concat
                                               (string-join
                                                (reverse (cdr (reverse sel)))
                                                ", ")
                                               ", et "
                                               (-last-item sel)))))))

;;;###autoload
(defun durand-take-param (url)
  "Take the last part of an youtube URL."
  (let ((beg (progn
               (string-match "\\?v=" url)
               (match-end 0))))
    (substring-no-properties url beg)))

;;;###autoload
(defun org-kill-youtube ()
  "Kill an org entry corresponding to a youtube link"
  (interactive)
  (let* (cands)
    (with-current-file "/Users/durand/org/notes.org" nil
      (setf cands (org-map-entries (lambda ()
                                     (durand-org-link-info t))
                                   "youtube")))
    (let ((liste-de-choix
           (mapcar (lambda (x)
                     (assoc-default x cands))
                   (durand-choose-list cands t "Chois une vidéo: "))))
      (with-current-file "/Users/durand/org/notes.org" nil
        (dolist (x liste-de-choix)
          (goto-char x)
          (org-cut-subtree))))))

;;;###autoload
(defun durand-org-filter-dates (str)
  "Filter out the timestamp in string"
  (if (string-match org-ts-regexp3 str)
      (replace-match "" nil nil str)
    str))

;;;###autoload
(defun durand-org-open-link (str)
  "Since `org-open-link-from-string' does not handle links with brackets correctly, this function
attempts to handle them."
  ;; HACK: PDF-view will error out if I open a pdf file while already inside a
  ;; pdf buffer. So I first switch to a non-pdf buffer. In any case, since a
  ;; buffer whose name begins with a space will not be inside the usual buffer
  ;; list after we switch buffers again, this switching will not have any effect
  ;; to the user.
  (let ((orig-buffer (current-buffer)))
    (switch-to-buffer " *server*")
    (condition-case err
        (org-link-open-from-string
         (org-link-make-string
          (org-link-decode (if (stringp str)
                               str
                             (car str)))
          "fake link"))
      ((error user-error)
       (switch-to-buffer orig-buffer)
       (user-error "%s" err)))))

;; Escape bracket chars as well
;; (add-to-list org-link-escape-chars 20)


;;; HACK: the original open link function is broken over links with spaces, and here is a fix.
;;;###autoload
(defun org-open-at-point-decoded (&optional arg reference-buffer)
  "Open link, timestamp, footnote or tags at point.

When point is on a link, follow it.  Normally, files will be
opened by an appropriate application.  If the optional prefix
argument ARG is non-nil, Emacs will visit the file.  With
a double prefix argument, try to open outside of Emacs, in the
application the system uses for this file type.

When point is on a timestamp, open the agenda at the day
specified.

When point is a footnote definition, move to the first reference
found.  If it is on a reference, move to the associated
definition.

When point is on a headline, display a list of every link in the
entry, so it is possible to pick one, or all, of them.  If point
is on a tag, call `org-tags-view' instead.

When optional argument REFERENCE-BUFFER is non-nil, it should
specify a buffer from where the link search should happen.  This
is used internally by `org-open-link-from-string'.

On top of syntactically correct links, this function also tries
to open links and time-stamps in comments, node properties, and
keywords if point is on something looking like a timestamp or
a link."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
    (let* ((context
            ;; Only consider supported types, even if they are not the
            ;; closest one.
            (org-element-lineage
             (org-element-context)
             '(clock comment comment-block footnote-definition
                     footnote-reference headline inline-src-block inlinetask
                     keyword link node-property planning src-block timestamp)
             t))
           (type (org-element-type context))
           (value (org-element-property :value context)))
      (cond
       ((not type) (user-error "No link found"))
       ;; No valid link at point.  For convenience, look if something
       ;; looks like a link under point in some specific places.
       ((memq type '(comment comment-block node-property keyword))
        (call-interactively #'org-open-at-point-global))
       ;; On a headline or an inlinetask, but not on a timestamp,
       ;; a link, a footnote reference.
       ((memq type '(headline inlinetask))
        (org-match-line org-complex-heading-regexp)
        (if (and (match-beginning 5)
                 (>= (point) (match-beginning 5))
                 (< (point) (match-end 5)))
            ;; On tags.
            (org-tags-view arg (substring (match-string 5) 0 -1))
          ;; Not on tags.
          (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
            (`(nil . ,_)
             (require 'org-attach)
             (org-attach-reveal 'if-exists))
            (`(,links . ,links-end)
             (dolist (link (if (stringp links) (list links) links))
               (search-forward link nil links-end)
               (goto-char (match-beginning 0))
               (org-open-at-point))))))
       ;; On a footnote reference or at definition's label.
       ((or (eq type 'footnote-reference)
            (and (eq type 'footnote-definition)
                 (save-excursion
                   ;; Do not validate action when point is on the
                   ;; spaces right after the footnote label, in order
                   ;; to be on par with behavior on links.
                   (skip-chars-forward " \t")
                   (let ((begin
                          (org-element-property :contents-begin context)))
                     (if begin (< (point) begin)
                       (= (org-element-property :post-affiliated context)
                          (line-beginning-position)))))))
        (org-footnote-action))
       ;; On a planning line.  Check if we are really on a timestamp.
       ((and (eq type 'planning)
             (org-in-regexp org-ts-regexp-both nil t))
        (org-follow-timestamp-link))
       ;; On a clock line, make sure point is on the timestamp
       ;; before opening it.
       ((and (eq type 'clock)
             value
             (>= (point) (org-element-property :begin value))
             (<= (point) (org-element-property :end value)))
        (org-follow-timestamp-link))
       ((eq type 'src-block) (org-babel-open-src-block-result))
       ;; Do nothing on white spaces after an object.
       ((>= (point)
            (save-excursion
              (goto-char (org-element-property :end context))
              (skip-chars-backward " \t")
              (point)))
        (user-error "No link found"))
       ((eq type 'inline-src-block) (org-babel-open-src-block-result))
       ((eq type 'timestamp) (org-follow-timestamp-link))
       ((eq type 'link)
        (let* ((type (org-element-property :type context))
               ;; NOTE: I changed this part.
               (path (cond
                      ;; ((string-prefix-p "http" type)
                      ;;  (concat
                      ;;   "https:"
                      ;;   (org-link-decode (org-element-property :path context))))
                      (t (org-link-decode (org-element-property :path context))))))
          ;; Switch back to REFERENCE-BUFFER needed when called in
          ;; a temporary buffer through `org-open-link-from-string'.
          (with-current-buffer (or reference-buffer (current-buffer))
            (cond
             ((equal type "file")
              (if (string-match "[*?{]" (file-name-nondirectory path))
                  (dired path)
                ;; Look into `org-link-parameters' in order to find
                ;; a DEDICATED-FUNCTION to open file.  The function
                ;; will be applied on raw link instead of parsed link
                ;; due to the limitation in `org-add-link-type'
                ;; ("open" function called with a single argument).
                ;; If no such function is found, fallback to
                ;; `org-open-file'.
                (let* ((option (org-element-property :search-option context))
                       (app (org-element-property :application context))
                       (dedicated-function
                        (org-link-get-parameter
                         (if app (concat type "+" app) type)
                         :follow)))
                  (if dedicated-function
                      (funcall dedicated-function
                               (concat path
                                       (and option (concat "::" option))))
                    (apply #'org-open-file
                           path
                           (cond (arg)
                                 ((equal app "emacs") 'emacs)
                                 ((equal app "sys") 'system))
                           (cond ((not option) nil)
                                 ((string-match-p "\\`[0-9]+\\'" option)
                                  (list (string-to-number option)))
                                 (t (list nil option))))))))
             ((functionp (org-link-get-parameter type :follow))
              (funcall (org-link-get-parameter type :follow) path))
             ((member type '("coderef" "custom-id" "fuzzy" "radio"))
              (unless (run-hook-with-args-until-success
                       'org-open-link-functions path)
                (if (not arg) (org-mark-ring-push)
                  (switch-to-buffer-other-window
                   (org-get-buffer-for-internal-link (current-buffer))))
                (let ((destination
                       (org-with-wide-buffer
                        (if (equal type "radio")
                            (org-search-radio-target
                             (org-element-property :path context))
                          (org-link-search
                           (pcase type
                             ("custom-id" (concat "#" path))
                             ("coderef" (format "(%s)" path))
                             (_ path))
                           ;; Prevent fuzzy links from matching
                           ;; themselves.
                           (and (equal type "fuzzy")
                                (+ 2 (org-element-property :begin context)))))
                        (point))))
                  (unless (and (<= (point-min) destination)
                               (>= (point-max) destination))
                    (widen))
                  (goto-char destination))))
             (t (browse-url-at-point))))))
       (t (user-error "No link found")))))
  (run-hook-with-args 'org-follow-link-hook))

;;;###autoload
(defmacro with-current-file (file-name &optional buffer-name &rest form)
  "Visit the file FILE-NAME and make the buffer current to execute FORM.
If emacs is not currently visiting the file FILE-NAME, then close the buffer afterwards.
If BUFFER-NAME is nil, then it defaults to the name of the file without directory."
  (declare (indent defun))
  `(let* ((nom_du_tampon_actuel (buffer-name))
          (nom_du_tampon (or ,buffer-name (file-name-nondirectory ,file-name)))
          (déjà_ouvert (get-buffer nom_du_tampon))
          (inhibit-messages t))
     (ignore inhibit-messages)
     (find-file ,file-name)
     (switch-to-buffer nom_du_tampon_actuel)
     (unwind-protect
         (with-current-buffer nom_du_tampon
           ,@form)
       (with-current-buffer nom_du_tampon
         (save-buffer 0))
       (when (and (not déjà_ouvert) (get-buffer nom_du_tampon))
         (kill-buffer nom_du_tampon))
       (switch-to-buffer nom_du_tampon_actuel))))

;;;###autoload
(defun org-open-articles (&optional arg)
  "Open all articles, that is, entries in \"notes.org\" with \"a_voir\" tag.
If ARG is (4), then execute `durand-update-article'.
If ARG is (16), then open entries in \"notes.org\" with \"TO-THINK\" TODO keyword
If ARG is (64), then execute `(durand-update-article t)'."
  (interactive "P")
  (cond
   ((or (null arg) (equal arg '(16)))
    (let* ((tag (if (null arg) "a_voir-ARCHIVE" "TODO=\"TO-THINK\"-ARCHIVE"))
           (files '("/Users/durand/org/notes.org"
                    "/Users/durand/org/math_article_links.org"))
           cands)
      (setf cands
            (cl-loop for file in files
                     append
                     (with-current-file file nil
                       (org-map-entries
                        (lambda ()
                          (let ((orig (durand-org-link-info nil)))
                            (append
                             (list (durand-org-filter-dates (car orig)))
                             (cdr orig)
                             (list file))))
                        tag)))
            cands (reverse cands))
      (let ((liste-de-choix
             (let* ((ivy-height 7)
                    (sel
                     (durand-choose-list cands nil "Chois un article: "
                                         nil nil nil t)))
               (ignore ivy-height)
               (cl-loop
                for x in sel
                append
                (durand-choose-list
                 (nreverse
                  (cdr
                   (nreverse
                    (assoc-default x cands
                                   (lambda (sa sb)
                                     (string= (string-trim sa)
                                              (string-trim sb)))))))
                 t "Chois un lien: " nil t nil t)))))
        (mapc #'durand-org-open-link liste-de-choix)
        (delete-other-windows))))
   ((equal arg '(4))
    (durand-update-article))
   ((equal arg '(64))
    (durand-update-article t))
   (t
    (message "This ARG is not supported: %s" arg))))

;;;###autoload
(defun durand-update-article (&optional all)
  "Update the link to an article. The link comes from the most
recently stored link, so choose carefully the target to update.
If ALL is non-nil, then the range is articles with TO-THINK TODO
keyword but not archived, instead of A_VOIR."
  (interactive)
  (let* ((predicate
          (cond
           (all
            (lambda ()
              (let ((tags (cond ((cl-every 'stringp (org-get-tags (point) t))
                                 (org-get-tags (point) t))
                                (t (list "")))))
                (and
                 (cl-member "math" tags :test 'string=)
                 (not (cl-member "a_voir" tags :test 'string=))
                 (not (cl-member "ARCHIVE" tags :test 'string=))))))
           (t
            (lambda ()
              (let ((tags (cond ((cl-every 'stringp (org-get-tags (point) t))
                                 (org-get-tags (point) t))
                                (t (list ""))))
                    (todo (cond ((stringp (org-get-todo-state))
                                 (substring-no-properties (org-get-todo-state)))
                                (t ""))))
                (and
                 (string= "TO-THINK" todo)
                 (not (cl-member "ARCHIVE" tags :test 'string=))))))))
         (files '("/Users/durand/org/notes.org"
                  "/Users/durand/org/math_article_links.org"))
         cands)
    (setf cands
          (cl-loop
           for file in files
           append
           (with-current-file file nil
             (save-excursion
               (goto-char (point-min))
               (reverse
                (cl-loop while (re-search-forward org-heading-regexp nil t)
                         when (funcall predicate)
                         collect (cl-destructuring-bind (title . position) (durand-org-link-info t)
                                   (list (durand-org-filter-dates title)
                                         position
                                         file))))))))
    (let* ((choix (completing-read "Chois un titre à mettre à jour: " cands nil t)
            ;; (icomplete-vertical-do '(:height (/ (frame-height) 4))
            ;;     (completing-read "Chois un titre à mettre à jour: " cands
            ;;                      nil t))
            )
           (item (cl-assoc choix cands :test #'string=)))
      (with-current-file (caddr item) nil
        (goto-char (cadr item))
        (org-update-link)))))

;;;###autoload
(defun org-open-weblink (&optional arg)
  "Open all weblink, that is, entries in \"notes.org\" with \"web_link\" tag.
If ARG is nil, then execute `durand-mark-weblink'
If ARG is '(4), then execute `durand-update-weblink'."
  (interactive "P")
  (cond
   ((null arg)
    (durand-mark-weblink))
   ((equal arg '(4))
    (durand-update-weblink))
   (t
    (message "This ARG is not supported: %s" arg))))

;;;###autoload
(defun durand-update-weblink ()
  "Update the link to a weblink;
the link comes from the most recently stored link, so choose carefully the target to update."
  (interactive)
  (let* ((tag "web_link-special-personnes-ARCHIVE")
         (files '("/Users/durand/org/notes.org" "/Users/durand/org/math_article_links.org"))
         cands)
    (setf cands (cl-loop for file in files
                         append (with-current-file file nil
                                  (org-map-entries
                                   (lambda ()
                                     (let ((orig (durand-org-link-info t)))
                                       (list (car orig) (cdr orig) file)))
                                   tag)))
          cands (mapcar (lambda (x)
                          (cons (durand-org-filter-dates (car x)) (cdr x)))
                        cands)
          cands (reverse cands))
    (let* ((choix (let ((icomplete-compute-delay 0))
                    (ignore icomplete-compute-delay)
                    (completing-read "Chois un lien à mettre à jour: " cands
                                     nil t)
                    ;; (icomplete-vertical-do '(:height (/ (frame-height) 4))
                    ;;   (completing-read "Chois un lien à mettre à jour: " cands
                    ;;                    nil t))
                    ))
           (item (cl-assoc choix cands :test #'string=))
           (link (plist-get org-store-link-plist :link))
           (desc (file-name-nondirectory
                  (or (plist-get org-store-link-plist :description)
                      ""))))
      (with-current-file (caddr item) nil
        (goto-char (cadr item))
        (org-update-link link nil nil desc)))))

;;;###autoload
(defun durand-mark-weblink ()
  "Choose web links and execute `durand-mark-links' for each of them."
  (interactive)
  (let* ((files '("/Users/durand/org/notes.org" "/Users/durand/org/math_article_links.org"))
         (tag "web_link-special-personnes-ARCHIVE")
         (cands
          (cl-loop for file in files
                   append (with-current-file file nil
                             (org-map-entries
                              (lambda ()
                                (let ((orig (durand-org-link-info t)))
                                  (list (car orig)
                                        (cdr orig)
                                        file)))
                              tag))))
         (cands (mapcar (lambda (x)
                          (cons (durand-org-filter-dates (car x))
                                (cdr x)))
                        cands))
         (cands (nreverse cands))
         (choice-headings (let ((icomplete-compute-delay 0))
                            (ignore icomplete-compute-delay)
                            (durand-choose-list cands t "Chois un lien à marquer: " t)))
         (choice-elements (mapcar (lambda (x)
                                    (cl-assoc x cands :test #'string=))
                                  choice-headings)))
    (cl-loop for element in choice-elements
             do (with-current-file (caddr element) nil
                  (goto-char (cadr element))
                  (durand-mark-links)))))

;;;###autoload
(defun durand-mark-links ()
  "Mark the links in a heading.
visiter: visit the link.
marquer: mark as seen.
presque marquer: mark as mostly seen.
unmarquer: remove the seen and the mostly seen mark.
tuer: delete the link."
  (interactive)
  (let* ((next-heading-position (save-excursion
                                  (outline-next-heading)
                                  (point)))
         (contained-links (save-excursion
                            (cl-loop while (re-search-forward org-link-any-re next-heading-position t)
                                     collect (list (match-string-no-properties 2)
                                                   (match-string-no-properties 3)
                                                   (match-beginning 0)
                                                   (match-end 0)))))
         (chosen-links (let ((icomplete-compute-delay 0))
                         (ignore icomplete-compute-delay)
                         (durand-choose-list contained-links t "Chois un lien: " t t)))
         (action-list (list "visiter" "marquer" "presque marquer" "unmarquer" "tuer"))
         (chosen-elements (cl-loop for link in chosen-links
                                   collect (cl-assoc link contained-links :test #'string=)))
         (chosen-action (completing-read "Quoi faire? " action-list nil t)))
    (cond
     ((string= chosen-action "visiter")
      (cl-loop for element in chosen-elements
               do (durand-org-open-link (car element))))
     ((string= chosen-action "unmarquer")
      (cl-loop for element in (cl-sort chosen-elements
                                       (lambda (x y)
                                         (>= (nth 2 x)
                                             (nth 2 y))))
               when (string-match " (seen)$\\| (mostly seen)$" (cadr element))
               do (let* ((link-url (car element))
                         (link-desc (replace-match "" nil nil (cadr element)))
                         (new-link (org-make-link-string link-url link-desc)))
                    (setf (buffer-substring (nth 2 element)
                                            (nth 3 element))
                          new-link))))
     ((string= chosen-action "presque marquer")
      (cl-loop for element in (cl-sort chosen-elements
                                       (lambda (x y)
                                         (>= (nth 2 x)
                                             (nth 2 y))))
               do (let* ((link-url (car element))
                         (orig-desc (cadr element))
                         (link-desc (cond
                                     ((string-match " (seen)$" orig-desc)
                                      (replace-match " (mostly seen)" nil nil orig-desc))
                                     ((string-match " (mostly seen)$" orig-desc)
                                      orig-desc)
                                     (t
                                      (concat orig-desc " (mostly seen)"))))
                         (new-link (org-make-link-string link-url link-desc)))
                    (setf (buffer-substring (nth 2 element)
                                            (nth 3 element))
                          new-link))))
     ((string= chosen-action "marquer")
      (cl-loop for element in (cl-sort chosen-elements
                                       (lambda (x y)
                                         (>= (nth 2 x)
                                             (nth 2 y))))
               do (let* ((link-url (car element))
                         (link-desc (cond
                                     ((string-match " (seen)$" (cadr element))
                                      (cadr element))
                                     ((string-match " (mostly seen)$" (cadr element))
                                      (replace-match " (seen)" nil nil (cadr element)))
                                     (t
                                      (concat (cadr element) " (seen)"))))
                         (new-link (org-make-link-string link-url link-desc)))
                    (setf (buffer-substring (nth 2 element)
                                            (nth 3 element))
                          new-link))))
     ((string= chosen-action "tuer")
      (cl-loop for element in (cl-sort chosen-elements
                                       (lambda (x y)
                                         (>= (nth 2 x)
                                             (nth 2 y))))
               do (delete-region (nth 2 element) (nth 3 element))))
     (t
      (user-error "This should not happen: illegal action: %s" chosen-action)))))

;;;###autoload
(defun org-agenda-jump-to-novels ()
  "Jump to one of novels"
  (interactive)
  (org-agenda-jump-to-item "最新章"))

;;;###autoload
(defun durand-org-modify-note ()
  "Modify the note entry at point."
  (interactive)
  (cl-assert (get-buffer "*durand-org-view-notes*") nil "No notes buffer alive")
  (let* ((note-choices
          (with-current-buffer "*durand-org-view-notes*"
            (org-map-entries (lambda ()
                               (list (buffer-substring-no-properties
                                      (+ 2 (point))
                                      (line-end-position))
                                     (get-text-property (point) :note-meta))))))
         (choice (completing-read "Which note to modify? " note-choices
                                  nil t))
         (choice-meta (cadr (assoc choice note-choices))))
    (goto-char (cadr choice-meta))))

;;;###autoload
(defun durand-org-delete-note ()
  "Delete the note entry at point"
  (interactive)
  (cl-assert (get-buffer "*durand-org-view-notes*") nil "No notes buffer alive")
  (let* ((note-choices
          (with-current-buffer "*durand-org-view-notes*"
            (org-map-entries (lambda ()
                               (list (buffer-substring-no-properties
                                      (+ 2 (point))
                                      (line-end-position))
                                     (get-text-property (point) :note-meta))))))
         (choice (completing-read "Which note to delete? " note-choices
                                  nil t))
         (choice-meta (cadr (assoc choice note-choices)))
         (beg (save-excursion
                (goto-char (car choice-meta))
                (forward-line -1)
                (point)))
         (end (1+ (cadr choice-meta))))
    (kill-region beg end)))

;;;###autoload
(defun org-clear-buffers ()
  "Clear all org mode buffers as well as /not needed/ buffers"
  (interactive)
  (progn
    (clean-up-buffers)
    (clean-up-buffers-regex "org$")))

;;;###autoload
(defmacro with-account (account-form)
  "Execute ACCOUNT-FORM only when we are visiting an account file."
  `(cond
    ((string-prefix-p "account" (buffer-name)) ,account-form)
    (t (user-error "\"%s\" is not an account file" (buffer-name)))))

;;;###autoload
(defun org-advance (x)
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org mode buffer"))
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (if (not x)
        (org-next-visible-heading 1)
      (org-forward-heading-same-level 1)))
  (org-narrow-to-subtree))

;;;###autoload
(defun org-retreat (x)
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org mode buffer"))
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (if (not x)
        (org-previous-visible-heading 1)
      (org-backward-heading-same-level 1)))
  (org-narrow-to-subtree))

;;;###autoload
(defun org-get-account-entries (&optional day month year span)
  "Get all entries.
DAY, MONTH, YEAR can be specified to gather previous entries to the given date;
otherwise, use the current date."
  (let ((day (or day (cadr (calendar-current-date))))
        (month (or month (car (calendar-current-date))))
        (year (or year (caddr (calendar-current-date))))
        (span (if (equal span -1) nil span))
        res)
    (if (or (not (integerp day)) (not (integerp month)) (not (integerp year)))
        (message "%s %s %s" (integerp day) (integerp month) (integerp year))
      (dolist (jour (org-find-all-days) (nreverse res))
        (let ((date-spec (org-date-to-gregorian (car jour))))
          (when (and (<= (cadr date-spec) day)
                     (or (null span) (< (- day (cadr date-spec)) span))
                     (= (car date-spec) month)
                     (= (caddr date-spec) year)
                     (< (- (cadr date-spec) day) 7))
            (push jour res)))))))

;;;###autoload
(defun org-calc-account (&optional arg)
  "Sum up entries

Non-nil ARG asks for day;

`C-uC-u' asks for day, month, year, and span.

If ARG is a positive integer, it is the span.

If span is not specified or -1, then it calculates all entries in the
same month before the given date."
  (interactive "P")
  (with-account
   (let* ((day (and arg (read-number "Day: " (cadr (calendar-current-date)))))
          (month (and (equal arg '(16)) (read-number "Month: " (car (calendar-current-date)))))
          (year (and (equal arg '(16)) (read-number "Year: " (caddr (calendar-current-date)))))
          (span (cond
                 ((and (integerp arg) (>= arg 0)) arg)
                 ((equal arg '(16)) (read-number "Span: " -1))
                 (t nil)))
          (entries (org-get-account-entries day month year span))
          (days (length entries))
          (total (let ((cur 0))
                   (save-excursion
                     (dolist (entry entries cur)
                       (goto-char (cdr entry))
                       (re-search-forward org-date-tree-headline-regexp)
                       (setq cur (+ cur (string-to-number
                                         (org-entry-get nil "total"))))))))
          (ave (ignore-errors (/ total days))))
     (message (concat
               (number-to-string days)
               " days, spent "
               (number-to-string total)
               " with average "
               (or
                (ignore-errors (number-to-string ave))
                "undefined"))))))

;;;###autoload
(defun org-find-all-days ()
  "Get all days information in TODO items
  The entry is supposed to be matched by `org-date-tree-headline-regexp`"
  (interactive)
  (with-account
   (let (res)
     (save-excursion
       (goto-char (point-min))
       (save-match-data
         (while (re-search-forward org-date-tree-headline-regexp nil t)
           (push (cons (match-string-no-properties 1)
                       (match-beginning 0))
                 res))))
     (nreverse res))))

(setq org-date-tree-headline-regexp "^\\*+\\s-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*$")

;;;###autoload
(defun org-find-last-day ()
  "Find the start position of last day entry"
  (interactive)
  (cdar (last (org-find-all-days))))

;;;###autoload
(defun org-find-pos-of-day (day)
  "Get the start position a specified day entry"
  (interactive)
  (let ((all-days (org-find-all-days)))
    (cdr (assoc day all-days))))

;;;###autoload
(defun org-update-account (&optional arg)
  "Update the last account.
With `C-u' prefix argument, update the chosen account;
with `C-uC-u' prefix argument, update all accounts."
  (interactive "P")
  (let* ((candidates (org-find-all-days))
         (choice
          (cond
           ((null arg) (last candidates))
           ((equal arg '(4)) (list (assoc
                                    (completing-read "Choose a day to update: " (mapcar #'car candidates)
                                                     nil t)
                                    candidates)))
           ((equal arg '(16)) candidates)
           (t 'wrong))))
    (if (eq choice 'wrong)
        (message "Only prefix args of the form \"nil\", \"C-u\", or \"C-uC-u\" are allowed")
      (save-excursion
        (dolist (date choice)
          (let ((total 0))
            (goto-char (cdr date))
            (re-search-forward org-date-tree-headline-regexp nil t)
            (org-map-tree (lambda ()
                            (setq total (+ total
                                           (string-to-number
                                            (or (org-entry-get (point) "cost")
                                                "0"))))))
            (org-set-property "total" (number-to-string total))
            (org-set-property "dashes" (make-string (if (> total 600) 60 (/ total 10)) ?-))))))))

;;;###autoload
(defun org-get-account-fields ()
  "Find all items under the current day"
  (interactive)
  (with-account
   (save-excursion
     (save-match-data
       (beginning-of-line)
       (unless (looking-at org-date-tree-headline-regexp)
         (re-search-forward org-date-tree-headline-regexp nil t -1))
       (re-search-forward org-date-tree-headline-regexp)
       (let (res pos)
         (org-map-tree (lambda ()
                         (setq pos (point))
                         (skip-chars-forward "* ")
                         (push (cons
                                (buffer-substring-no-properties (point) (line-end-position))
                                (list (org-entry-get (point) "cost") pos))
                               res)))
         (cdr (nreverse res)))))))

;;;###autoload
(defun org-modify-account ()
  "Modify the account entries under the current day"
  (interactive)
  (with-account
   (let* ((account-field-list (org-get-account-fields))
          (target-entry (completing-read "Choose one entry: "
                                         (mapcar #'car account-field-list)
                                         nil t))
          (target-position (caddr (assoc target-entry account-field-list)))
          (target-item (read-string "New item: " target-entry))
          (target-price (read-string "New price: "
                                     (cadr (assoc target-entry account-field-list))))
          (note-change-p (y-or-n-p "Change note?")))
     (goto-char target-position)
     (skip-chars-forward "* ")
     (re-search-forward ".*$")
     (replace-match target-item)
     (org-set-property "cost" target-price)
     (when note-change-p
       (org-end-of-meta-data t)))))

;;;###autoload
(defun org-delete-account ()
  "Delete the account entries under the current day"
  (interactive)
  (with-account
   (let* ((account-field-list (org-get-account-fields))
          (target-entry (completing-read "Choose one entry: "
                                         (mapcar #'car account-field-list)
                                         nil t))
          (target-position (caddr (assoc target-entry account-field-list))))
     (goto-char target-position)
     (skip-chars-forward "* ")
     (org-mark-subtree)
     (delete-region (region-beginning) (region-end)))))

;;;###autoload
(defun org-set-account-according-to-date (date &optional _month _year)
  "Update accounts tag according to DATE.
  DATE is an integer representing a date in month MONTH and year YEAR.
  MONTH and YEAR default to the current ones.
  This means if a date has the same quotient as DATE when
  divided by 7, then it will be tagged `account';
  otherwise it will have no tags."
  (with-account
   (save-excursion
     (outline-show-all)
     (dolist (running-day (org-find-all-days))
       (goto-char (cdr running-day))
       (re-search-forward org-date-tree-headline-regexp nil t)
       (let ((day (cadr (org-date-to-gregorian (car running-day)))))
         (org-set-tags-to (cond ((and (<= day date)
                                      (> (+ 7 day) date)) ":account:")
                                (t nil)))
         (org-set-tags t t)))))
  (outline-hide-body))

;;;###autoload
(defun org-day-format-transform (day &optional month year)
  "Take an integer DAY and transform it to a string.
  For example,
  (org-day-format-transform 1)
  when executed in August 2018 becomes
  => \"2018-08-01\""
  (let* ((day-string (pad-string-to (format "%d" day) 2))
         (padded-date-string-list (mapcar (lambda (x) (pad-string-to (format "%d" x) 2))
                                          (calendar-current-date)))
         (month (or (and month (pad-string-to (format "%d" month) 2))
                    (car padded-date-string-list)))
         (year (or (and year (pad-string-to (format "%d" year) 2))
                   (caddr padded-date-string-list))))
    (concat year
            "-"
            month
            "-"
            day-string)))

;;;###autoload
(defun pad-string-to (str num &optional material backp)
  "Pad a string STR to be of length greater than or equal to NUM with 0.
If INITIAL is set, use that to pad; if BACKP, then pad at the end."
  (cond ((< (length str) num)
         (if (null backp)
             (concat (make-string (- num (length str)) (or material ?0)) str)
           (concat str (make-string (- num (length str)) (or material ?0)))))
        (t
         str)))

;;;###autoload
(defun org-run-src-block ()
  "Search for a src block and run it"
  (interactive)
  (with-account
   (save-excursion
     (re-search-forward "BEGIN_SRC")
     (org-babel-execute-src-block))))

;;;###autoload
(defun org-set-item-price-note (item-name item-price item-note)
  (interactive (let ((item (completing-read "Enter item: "
                                            '("breakfast" "brunch" "brunverage"
                                              "lunch" "dinner" "beverage")))
                     (price (read-number "Enter price: " 0))
                     (note (read-string "Enter note: " nil nil "todo")))
                 (list item price note)))
  (with-account
   (progn (goto-char (point-max))
          (outline-show-all)
          (re-search-backward "tblfm")
          (forward-line -1)
          (org-table-insert-row 1)
          (org-table-insert-hline)
          (org-table-put (org-table-current-line) (org-table-current-column) item-name)
          (org-table-put (org-table-current-line) (1+ (org-table-current-column)) (number-to-string item-price))
          (org-table-put (org-table-current-line) (+ 2 (org-table-current-column)) item-note t)
          (outline-hide-body))))

;;;###autoload
(defun org-delete-item-price-note (row-num &optional _total-num)
  (interactive (let* ((total-num (save-excursion
                                   (goto-char (point-max))
                                   (outline-show-all)
                                   (re-search-backward "tblfm")
                                   (forward-line -2)
                                   (org-table-current-line)))
                      (num (completing-read "Enter row number: "
                                            (mapcar #'number-to-string (number-sequence 1 total-num)))))
                 (list (string-to-number num) total-num)))
  (with-account
   (progn
     (goto-char (point-max))
     (outline-show-all)
     (re-search-backward "tblfm")
     (forward-line -2)
     (org-table-goto-line row-num)
     (kill-whole-line 2)
     (outline-hide-body))))

;;;###autoload
(defun org-account-go-to-day (day &optional no-narrowp)
  "Go to the position of day DAY"
  (interactive (list (completing-read "DAY: "
                                      (if current-prefix-arg
                                          (org-find-all-days)
                                        (save-restriction
                                          (widen)
                                          (org-find-all-days))))
                     current-prefix-arg))
  (with-account
   (if no-narrowp
       (progn
         (outline-hide-sublevels 2)
         (goto-char (org-find-pos-of-day day))
         (outline-show-children)
         (recenter-top-bottom 0))
     (progn
       (widen)
       (outline-hide-sublevels 2)
       (goto-char (org-find-pos-of-day day))
       (recenter-top-bottom 0)
       (org-narrow-to-subtree)
       (outline-show-children)))))

;;;###autoload
(defun org-account-go-to-last-day (&optional arg)
  "Go to the position of day DAY"
  (interactive "P")
  (if arg
      (with-account
       (progn
         (goto-char (org-find-last-day))
         (outline-show-children)
         (recenter-top-bottom 0)))
    (with-account
     (progn
       (widen)
       (outline-hide-sublevels 2)
       (goto-char (org-find-last-day))
       (outline-show-children)
       (recenter-top-bottom 0)
       (org-narrow-to-subtree)))))

;;;###autoload
(defun find-next-link-in-buffer (&optional arg)
  "
  Navigate to the links in the buffer \"without setting marks\";

  If ARG is nil, then go to the next link.
  If ARG is non-nil, then it is interpreted according to the interactive form \"p\""
  (interactive "p")
  (let ((search-count (or arg 1)))
    (re-search-forward "\\[\\[[^][]+]\\[[^][]+]]" nil t search-count)
    (backward-char 1)))

;;;###autoload
(defun find-previous-link-in-buffer (&optional arg)
  "
  Navigate to the links in the buffer \"without setting marks\";

  If ARG is nil, then go to the previous link.
  If ARG is non-nil, then it is interpreted according to the interactive form \"p\"

  This is a convenient variant of `find-next-link-in-buffer'"
  (interactive "p")
  (let ((search-count (or arg 1)))
    (re-search-backward "\\[\\[[^][]+]\\[[^][]+]]" nil t search-count)
    (forward-char 1)))

;; Last week day of the month
;; it's the last day of the month & it is a weekday
;; it's a friday, and it's the last-but-one or last-but-two day
;; of the month
;;;###autoload
(defun last-week-day-of-month-p (date)
  (let* ((day-of-week (calendar-day-of-week date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-month-day (calendar-last-day-of-month month year))
         (month-day (cadr date)))
    (or (and (eq month-day last-month-day)
             (memq day-of-week '(1 2 3 4 5)))
        (and (eq day-of-week 5)
             (or (eq month-day
                     (1- last-month-day))
                 (eq month-day
                     (- last-month-day 2)))))))

;; select week days
;;;###autoload
(defun selected-week-days (list-of-days date)
  "Return true if the date is in one of the days of week"
  (let ((day-of-week (calendar-day-of-week date)))
    (not (null
          (memq day-of-week list-of-days)))))

;; durand-capture
;;;###autoload
(defun durand-capture ()
  "Use `completing-read' to choose a key."
  (interactive)
  (let* ((temps (org-contextualize-keys
                 (org-capture-upgrade-templates org-capture-templates)
                 org-capture-templates-contexts))
         (choix (mapcar (lambda (x)
                          (cons (string-join (list (car x)
                                                   (cadr x))
                                             ": ")
                                (car x)))
                        temps))
         (clé (assoc-default ;; (minibuffer-with-setup-hook 'durand-headlong-minibuffer-setup-hook
                             ;;   (completing-read "Chois un clé: " choix
                             ;;                    nil t "^"))

                             (ivy-read "Chois un clé: " choix
                                       :require-match t
                                       :caller 'durand-capture
                                       :update-fn 'durand-self-insert-complete-and-exit
                                       :unwind 'reset-durand-changed
                                       :initial-input "^")
                             choix)))
    (org-capture nil clé)))

;;;###autoload
(defun durand-capture-web-link ()
  "Since `org-capture' sometimes behaves weirdly, I make my own capturing function.
This should be run after the store-link properties have been set."
  (interactive)
  (let* ((template (nth 3 (alist-get
                           "L"
                           (org-capture-upgrade-templates org-capture-templates)
                           nil nil #'string=)))
         (template (org-capture-fill-template template))
         (cur-buffer (current-buffer)))
    (org-determine-link-file)
    (let ((orig-p (point)))
      (org-paste-subtree 1 template)
      (goto-char orig-p))
    (when (search-forward "%?" nil t)
      (replace-match ""))
    (save-buffer 0)
    (unless (eq cur-buffer (current-buffer))
      (kill-buffer (current-buffer)))))

(after! org-pdfview
  ;; custom store link function to store the height as well
;;;###autoload
  (defun org-pdfview-store-link ()
    "Store a link to a pdfview buffer."
    (when (derived-mode-p 'pdf-view-mode)
      ;; This buffer is in pdf-view-mode
      (let* ((path buffer-file-name)
             (page (pdf-view-current-page))
             (height (let ((ori (substring-no-properties (pdf-misc-size-indication) 1)))
                       (cond
                        ((string= ori "Bot")
                         ;; NOTE: if Bot I cannot know precisely the percentage.
                         ;; So return -1 to signal to scroll up a little and ask
                         ;; again.
                         (pdf-view-previous-line-or-previous-page 1)
                         (replace-regexp-in-string
                          "%%" ""
                          (substring-no-properties (pdf-misc-size-indication) 1)))
                        ((string= ori "Top")
                         nil)
                        (t
                         (if (string-match "%%" ori)
                             (replace-match "" nil nil ori)
                           ori)))))
             (real-height (when height
                            (number-to-string (/ (string-to-number height) 100.0))))
             (link (concat "pdfview:" path "::" (number-to-string page)
                           (when height (concat "++" real-height)))))
        (org-store-link-props
         :type "pdfview"
         :link link
         :description path)))))

;;; archive advise

;;;###autoload
(defadvice! durand-archive-save-and-kill (old-fun &optional find-done)
  "Save and kill the buffer after archiving."
  :around 'org-archive-subtree
  (let* ((location (org-archive--compute-location
                    (or (org-entry-get nil "ARCHIVE" 'inherit)
                        org-archive-location)))
         (archive-buffer-name (file-name-nondirectory (car location)))
         (opened (get-buffer archive-buffer-name)))
    (funcall old-fun find-done)
    (unless opened
      (when (get-buffer archive-buffer-name)
        (with-current-buffer archive-buffer-name
          (ignore-errors (save-buffer 0)))
        (kill-buffer archive-buffer-name)))))

;;; fine-tune org-tree-slides
;;;
;;; NOTE: Originally the narrowing effect will exclude the first character of
;;; the sub-tree from the display, so we fix this problem first.

;; first clear the original advice

(use-package! org-tree-slide
  :commands org-tree-slide-mode
  :config
  ;; modify that advice
;;;###autoload
  (defadvice! durand-org-present--narrow-to-subtree-a (orig-fn &rest args)
    "Narrow to the target subtree when you start the presentation."
    :around #'org-tree-slide--display-tree-with-narrow
    (advice-remove 'org-tree-slide--display-tree-with-narrow
                   '+org-present--narrow-to-subtree-a)
    (cl-letf (((symbol-function #'org-narrow-to-subtree)
               (lambda ()
                 (save-excursion
                   (save-match-data
                     (org-with-limited-levels
                      (narrow-to-region
                       (progn
                         (when (org-before-first-heading-p)
                           (org-next-visible-heading 1))
                         ;; (ignore-errors (org-up-heading-all 99))
                         ;; (forward-line 1)
                         ;; (forward-char -1)
                         (point))
                       (progn (org-end-of-subtree t t)
                              (when (and (org-at-heading-p) (not (eobp)))
                                (backward-char 1))
                              (point)))))))))
      (apply orig-fn args)))

;;; Now we tackle the problem of the big font and skipping level.

  ;; It turns out this is controlled by the variable `+org-present-text-scale'.

  (setf +org-present-text-scale 1)

  ;; to set up skipping level correctly we shall use a transient advice

  (defadvice! durand-org-tree-slide-mode-set-var-a (&rest _args)
    "Set up some settings and remove this advice."
    :before 'org-tree-slide-mode
    (setf org-tree-slide-skip-outline-level 4
          org-tree-slide-modeline-display t)
    (advice-remove 'org-tree-slide-mode
                   'durand-org-tree-slide-mode-set-var-a)))

;;; It turns out I have to re-define the function `org-entry-put' for it to
;;; function properly.

;;;###autoload
(defun org-entry-put (pom property value)
  "Set PROPERTY to VALUE for entry at point-or-marker POM.

If the value is nil, it is converted to the empty string.  If it
is not a string, an error is raised.  Also raise an error on
invalid property names.

PROPERTY can be any regular property (see
`org-special-properties').  It can also be \"TODO\",
\"PRIORITY\", \"SCHEDULED\" and \"DEADLINE\".

For the last two properties, VALUE may have any of the special
values \"earlier\" and \"later\".  The function then increases or
decreases scheduled or deadline date by one day.

Modified by Durand <2020-04-28 Mar 14:55>"
  (cond ((null value) (setq value ""))
	      ((not (stringp value)) (error "Properties values should be strings"))
	      ((not (org--valid-property-p property))
	       (user-error "Invalid property name: \"%s\"" property)))
  (org-with-point-at pom
    (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
	      (org-back-to-heading-or-point-min t)
      (org-with-limited-levels (org-back-to-heading-or-point-min t)))
    (let ((beg (point)))
      (cond
       ((equal property "TODO")
	      (cond ((not (org-string-nw-p value)) (setq value 'none))
	            ((not (member value org-todo-keywords-1))
	             (user-error "\"%s\" is not a valid TODO state" value)))
	      (org-todo value)
	      (org-align-tags))
       ((equal property "PRIORITY")
	      (org-priority (if (org-string-nw-p value) (string-to-char value) ?\s))
	      (org-align-tags))
       ((equal property "SCHEDULED")
	      (forward-line)
	      (if (and (looking-at-p org-planning-line-re)
		             (re-search-forward
		              org-scheduled-time-regexp (line-end-position) t))
	          (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		              ((string= value "later") (org-timestamp-change 1 'day))
		              ((string= value "") (org-schedule '(4)))
		              (t (org-schedule nil value)))
	        (if (member value '("earlier" "later" ""))
	            (call-interactively #'org-schedule)
	          (org-schedule nil value))))
       ((equal property "DEADLINE")
	      (forward-line)
	      (if (and (looking-at-p org-planning-line-re)
		             (re-search-forward
		              org-deadline-time-regexp (line-end-position) t))
	          (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		              ((string= value "later") (org-timestamp-change 1 'day))
		              ((string= value "") (org-deadline '(4)))
		              (t (org-deadline nil value)))
	        (if (member value '("earlier" "later" ""))
	            (call-interactively #'org-deadline)
	          (org-deadline nil value))))
       ((member property org-special-properties)
	      (error "The %s property cannot be set with `org-entry-put'" property))
       (t
	      (let* ((range (org-get-property-block beg 'force))
	             (end (cdr range))
	             (case-fold-search t))
	        (goto-char (car range))
          ;; NOTE: Added by Durand
	        (cond
           ((re-search-forward (org-re-property property nil t) end t)
            (let ((match-beg (match-beginning 0))
                  (match-fin (match-end 0)))
              (delete-region match-beg match-fin)
              (goto-char match-beg)))
           (t
            (goto-char end)
            (newline)
            (forward-char -1)))
          ;; NOTE: Commented by Durand
          ;; (if (re-search-forward (org-re-property property nil t) end t)
	        ;;     (progn (delete-region (match-beginning 0) (match-end 0))
		      ;;            (goto-char (match-beginning 0)))
	        ;;   (goto-char end)
	        ;;   (insert "\n")
	        ;;   (backward-char))
	        (insert ":" property ":")
	        (when value (insert " " value))
	        (org-indent-line)))))
    (run-hook-with-args 'org-property-changed-functions property value)))

;;; quickly edit using org-noter

;;;###autoload
(defun durand-org-noter-edit-document ()
  "If needed, get out of the maximized window and edit the notes buffer."
  (interactive)
  (when (featurep! :lang org +noter)
    (org-noter--with-valid-session
     (when doom--enlargen-last-wconf
       (doom/window-enlargen))
     (select-window (org-noter--get-notes-window))
     (doom/window-enlargen))))

;;;###autoload
(defun durand-org-noter-go-to-doc ()
  "Go back to the document."
  (interactive)
  (when (featurep! :lang org +noter)
    (org-noter--with-valid-session
     (when doom--enlargen-last-wconf
       (doom/window-enlargen))
     (select-window (org-noter--get-doc-window))
     (doom/window-enlargen))))

;;;###autoload
(defun durand-view-svg (graph-file)
  "View the graph file iusing Safari"
  (start-process "view" nil "open"
                 "-a" "Safari" graph-file))

;;;###autoload
(defun durand-org-archive-file-name (file-name)
  "Produce an archive file name.
The rule is as follows:
example.org
example.org_archive
example(1).org_archive
example(2).org_archive
etc."
  (unless (or (string-match "org$" file-name)
              (string-match "org_archive$" file-name))
    (user-error "Not an org file."))
  (cond
   ((string-match "org$" file-name)
    (replace-match "org_archive" nil nil file-name))
   ((string-match "\\((\\([[:digit:]]+\\))\\).org_archive$" file-name)
    (replace-match
     (format "(%d)"
             (1+ (string-to-number
                  (match-string-no-properties 2 file-name))))
     nil nil file-name 1))
   (t
    (replace-regexp-in-string ".org_archive$" "(1).org_archive" file-name))))

;;;###autoload
(defun durand-org-goto-archive ()
  "Go to the archive file of the current org file, if any.
It will cycle through all archive files of the file.
The rule is as follows:
example.org
example.org_archive
example(1).org_archive
example(2).org_archive
etc."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org file."))
  (let* ((current-name (buffer-file-name (current-buffer)))
         (base-name (cond
                     ((string-match "org_archive$" current-name)
                      (replace-match "org" nil nil current-name))
                     (t current-name)))
         (base-name (cond
                     ((string-match "\\(([[:digit:]])\\).org$" base-name)
                      (replace-match "" nil nil base-name 1))
                     (t base-name)))
         (next-name (durand-org-archive-file-name current-name))
         (next-name (cond
                     ((file-exists-p next-name) next-name)
                     (t base-name))))
    (find-file next-name)))

;; NOTE: Modified by durand to use `completing-read-multiple' instead of
;; `completing-read'.
;;;###autoload
(defun org-set-tags-command (&optional arg)
  "Set the tags for the current visible entry.

When called with `\\[universal-argument]' prefix argument ARG, \
realign all tags
in the current buffer.

When called with `\\[universal-argument] \\[universal-argument]' prefix argument, \
unconditionally do not
offer the fast tag selection interface.

If a region is active, set tags in the region according to the
setting of `org-loop-over-headlines-in-active-region'.

This function is for interactive use only;
in Lisp code use `org-set-tags' instead."
  (interactive "P")
  (let ((org-use-fast-tag-selection
	       (unless (equal '(16) arg) org-use-fast-tag-selection)))
    (cond
     ((equal '(4) arg) (org-align-tags t))
     ((and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let (org-loop-over-headlines-in-active-region) ;  hint: infinite recursion.
	      (org-map-entries
	       #'org-set-tags-command
	       nil
	       (if (eq org-loop-over-headlines-in-active-region 'start-level)
	           'region-start-level
	         'region)
	       (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))))
     (t
      (save-excursion
	      (org-back-to-heading)
	      (let* ((all-tags (org-get-tags))
	             (table (setq org-last-tags-completion-table
			                      (org--tag-add-to-alist
			                       (and org-complete-tags-always-offer-all-agenda-tags
				                          (org-global-tags-completion-table
				                           (org-agenda-files)))
			                       (or org-current-tag-alist (org-get-buffer-tags)))))
	             (current-tags
		            (cl-remove-if (lambda (tag) (get-text-property 0 'inherited tag))
			                        all-tags))
	             (inherited-tags
		            (cl-remove-if-not (lambda (tag) (get-text-property 0 'inherited tag))
				                          all-tags))
	             (tags
		            (replace-regexp-in-string
                 ;; Ignore all forbidden characters in tags.
		             "[^[:alnum:]_@#%]+" ":"
		             (if (or (eq t org-use-fast-tag-selection)
			                   (and org-use-fast-tag-selection
			                        (delq nil (mapcar #'cdr table))))
		                 (org-fast-tag-selection
		                  current-tags
		                  inherited-tags
		                  table
		                  (and org-fast-tag-selection-include-todo org-todo-key-alist))
		               (let ((org-add-colon-after-tag-completion (< 1 (length table))))
                     ;; I change this part
		                 (org-trim
                      (org-make-tag-string
                       (completing-read-multiple
                        "Tags: "
                        #'org-tags-completion-function
                        nil nil (replace-regexp-in-string
                                 ":" ","
                                 (org-make-tag-string current-tags))
                        'org-tags-history))))))))
	        (org-set-tags tags)))))
    ;; `save-excursion' may not replace the point at the right
    ;; position.
    (when (and (save-excursion (skip-chars-backward "*") (bolp))
	             (looking-at-p " "))
      (forward-char))))

;;;###autoload
(defvar durand-org-fill-column 90
  "The variable controlling the fill-column in Org buffers.
Set this locally to a different value so as to bypass hook problems.")

;;;###autoload
(defun durand-org-activate-auto-fill ()
  "Activate auto-fill-mode.
Set the fill-column to `durand-org-fill-column'."
  (setf fill-column durand-org-fill-column)
  (auto-fill-mode 1))

;;;###autoload
(defun durand-org-preparation ()
  "Prepare some settings to speed up org performance."
  (flycheck-mode -1)
  (spell-fu-mode -1))

;; FIXME: This is not working!
;;;###autoload
;; (defadvice! durand-org-noter-kill-session-buffers (&rest _args)
;;   "Kill unnecessary buffers."
;;   :before 'org-noter-kill-session
;;   (let ((doc-file-name
;;          (buffer-file-name
;;           (org-noter--session-doc-buffer org-noter--session))))
;;     (run-at-time "0.5 sec" nil
;;                  (lambda ()
;;                    (kill-buffer (get-file-buffer doc-file-name))))))

;;;###autoload
(defvar durand-org-capture-account-which-list
  '("breakfast" "brunch" "brunverage" "lunch" "dinner" "beverage" "snack" "fruit")
  "The list of purposes of accounts in the capture-template for accounting.")

;;;###autoload
(defun durand-org-capture-account-template ()
  "Org capture template for account."
  (let* ((shop-and-items (durand-org-complete-capture-account))
         (which (completing-read
                 "For what? "
                 durand-org-capture-account-which-list))
         (cost (number-to-string
                (read-number "Cost: " 0)))
         (from (completing-read "From: " '("Cash" "etique")))
         (active-time (format-time-string
                       (cdr org-time-stamp-formats)))
         (inactive-time
          (let ((temp active-time))
            (while (string-match "<\\|>" temp)
              (setf temp
                    (replace-match
                     (cond
                      ((string= (match-string-no-properties 0 temp) "<") "[")
                      ((string= (match-string-no-properties 0 temp) ">") "]"))
                     nil nil temp)))
            temp)))
    (format "%s\n  :PROPERTIES:\n  :cost: %s\n  :FROM: %s\n  :RECORD_TIME: %s\n  :END: \n  %s%%?"
            which cost from inactive-time shop-and-items)))

;;;###autoload
(defadvice! durand-org-capture-default-h ()
  "Define my own defaults."
  :override '+org-init-capture-defaults-h
  (setq org-capture-templates
        '(("m" "Account records" entry
           (file+olp+datetree "~/org/account/account.org")
           "* %(durand-org-capture-account-template)"
           ;; "* %^{ITEM|breakfast|brunch|brunverage|lunch|dinner|beverage|snack|fruit}\n  :PROPERTIES:\n  :cost: %(number-to-string (read-number \"COST:\" 0))\n  :FROM: %(completing-read \"FROM: \" '(\"Cash\" \"etique\"))\n  :RECORD_TIME: %U\n  :END:\n  %(durand-org-complete-capture-account)%?"
           :jump-to-captured t)
          ("d" "Record Diaries" entry
           (file+olp+datetree "~/org/diary.org")
           "* %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
           :jump-to-captured t)
          ("w" "Withdrawal records" entry
           (file+headline "~/org/wiki.org" "Money Withdrawal")
           "* WITHDRAW NTD %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil nil)\n"
           :kill-buffer t)
          ("l" "Store links" entry
           (file "/Users/durand/org/math_article_links.org")
           "* TO-THINK %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)\n%a\n" :kill-buffer t)
          ("g" "GNUS" entry
           (file "~/org/notes.org")
           "* TO-THINK %:subject\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n  %:from\n  %:to\n  %a\n  %?"
           :empty-lines 1
           :kill-buffer t)
          ("L" "for storing webpages" entry
           #'org-determine-link-file
           "* PENDING %(org-filter-title) %(org-determine-tag)\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n  %(org-filtered-link)\n  %i\n  %?"
           :empty-lines 1
           :kill-buffer t
           :immediate-finish t)
          ("t" "TODO" entry
           (file "~/org/aujourdhui.org")
           "* TODO %? %^{Date to do:}t\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
           :kill-buffer t)
          ("b" "Blog posts" entry
           (file+headline "~/org/notes.org" "Blog posts")
           "* %? %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
          ("a" "Abstractions" entry
           (file+headline "~/org/wiki.org" "Abstractions")
           "* ABSTRACT %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n")
          ("A" "Agenda" entry
           (file+headline "~/org/agenda.org" "Agenda")
           "* TODO %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :DURATION: %^{Date: }t\n  :END:\n\n")
          ("y" "YiFu" entry
           (file+headline "~/org/wiki.org" "Yi Fu Tips")
           "* MEMO %^{word}\n  :PROPERTIES:\n  :STORY: %\\2\n  :MEANING: %\\3\n  :END:\n** Yi Fu story\n   %^{story}\n** Meaning\n   %^{meaning}"
           :kill-buffer t
           :immediate-finish t)
          ("c" "Chansons" entry
           (file+headline "~/org/wiki.org" "Liste de Chansons")
           "* MEMO %^{title}\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :LINK: [[%^{link}][%^{description}]]\n  :END:\n  %?"
           :jump-to-captured t)
          ("f" "français" entry
           (file+headline "~/org/français/français.org" "Liste de mots français")
           "* MEMO %^{mot} :drill:\n  :PROPERTIES:\n  :DRILL_CARD_TYPE: français\n  :RECORD_TIME: %U\n  :MEANING: %^{ce qu'il veut dire}\n  :END:\n\n  MEANING: %\\2\n%?"
           :jump-to-captured t)))
  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        +org-capture-journal-file
        (expand-file-name +org-capture-journal-file org-directory))

  ;; Kill capture buffers by default (unless they've been visited)
  (after! org-capture
    (org-capture-put :kill-buffer t))

  ;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  ;; HACK Doom doesn't support `customize'. Best not to advertise it as an
  ;;      option in `org-capture's menu.
  (defadvice! +org--remove-customize-option-a (orig-fn table title &optional prompt specials)
    :around #'org-mks
    (funcall orig-fn table title prompt
             (remove '("C" "Customize org-capture-templates")
                     specials)))

  (defadvice! +org--capture-expand-variable-file-a (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    :filter-args #'org-capture-expand-file
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))

  (add-hook! 'org-capture-mode-hook
    (defun +org-show-target-in-capture-header-h ()
      (setq header-line-format
            (format "%s%s%s"
                    (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                'face 'font-lock-string-face)
                    org-eldoc-breadcrumb-separator
                    header-line-format))))

  (when (featurep! :editor evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state)))

;;;###autoload
(defun org-edit-special-h (&rest orig-args)
  "Make it full frame"
  (ignore orig-args)
  (delete-other-windows)
  (setf org-src--saved-temp-window-config
        (current-window-configuration)))
