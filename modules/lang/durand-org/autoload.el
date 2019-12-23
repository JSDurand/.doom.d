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
(defun recenter-to-top (&rest some)
  "Recenter to top"
  (interactive)
  (recenter 0))

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
     ((string-match "https?://www.uukanshu.com" link)
      ":roman:")
     ((string-match "https?://stacks.math.columbia.edu/" link)
      ":web_link:stack:")
     (t
      ":web_link:"))))

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
(defun durand-collect-shop-infos ()
  "Return relevant information from the heading."
  (when (= (car (org-heading-components)) 4)
    ;; It is possible to be called inside `org-map-entries'.
    (let* ((date-string (if (save-excursion
                              (outline-up-heading 1)
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
                     (dotimes (i (length ori) res)
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
         (splitted-list (mapcar #'string-to-number splitted)))
    (encode-time 0 0 0
                 (caddr splitted-list)
                 (cadr splitted-list)
                 (car splitted-list))))

;;;###autoload
(defun durand-account-match-last-unit (str &optional unit)
  "Match the last UNIT. UNIT can be `day', `week', `month', `year',
or a custom specifier of time period."
  (setf durand-account-report-period-str (format "%s" (or unit 'day)))
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
       ((pred stringp)
        (let* ((str-list (split-string unit ":"))
               (beg-str (car str-list))
               (end-str (cond
                         ((not (string= (cadr str-list) ""))
                          (cadr str-list))
                         (t
                          "+0")))
               (beg (org-read-date nil t beg-str "Chois le début:"))
               (end (org-read-date nil t end-str "Chois la fin:")))
          (and (time-less-p beg str-time)
               (time-less-p str-time end))))
       (_
        (user-error "Unknown UNIT: %s" unit))))))

;; convenient functions

;;;###autoload
(cl-defun durand-change-parameter (&key unit report-mode sum-type exclude-type)
  "general function to change the parameters of account reporting"
  (let* ((this-buffer (current-buffer))
         (this-window (selected-window))
         (account-buffer-name "account.org")
         cur-u cur-rm cur-st cur-et)
    (when (get-buffer "*ACCOUNT REPORT*")
      (switch-to-buffer "*ACCOUNT REPORT*")
      (goto-char (point-min))
      (setf cur-u (let* ((str (buffer-substring-no-properties (point) (line-end-position))))
                    (cond ((string= str "day") (intern str))
                          ((string= str "week") (intern str))
                          ((string= str "month") (intern str))
                          ((string= str "year") (intern str))
                          (t str)))
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
  (let ((beg (read-string "Le début: "))
        (end (read-string "La fin: ")))
    (durand-change-parameter :unit (string-join (list beg end) ":"))))

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
  (durand-change-parameter :report-mode (intern (ivy-read "Mode: " '("separate" "combine")
                                                          :require-match t))))

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
  (forward-char)
  (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t (- arg))
  (beginning-of-line))

;;;###autoload
(defun durand-view-go-to-account-day ()
  "go to the corresponding date"
  (interactive)
  (unless (string= (buffer-name) "*ACCOUNT REPORT*")
    (user-error "This should only be executed in account report buffer."))
  (cond
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
                  ((not (string-match org-link-bracket-regexp l))
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
          (while (not fin)
            (let ((pos (next-single-property-change (line-end-position) 'durand-agenda-regular-header)))
              (if pos
                  (progn
                    (setf num (1+ num)
                          res (append res (list pos)))
                    (goto-char pos))
                (setf fin t))))
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
                 (when chois
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
        (org-remove-subtree-entries-from-agenda buffer dbeg dend))
      (with-current-buffer buffer (delete-region dbeg dend))
      (message "Agenda item and source killed"))))

;; La fonction `org-agenda-set-mode-name' est carrément inutile!
;; La fonction originale est dans le fichier org-agenda.el
;;;###autoload
(defun org-agenda-set-mode-name ()
  "Cette fonction ne sert rien!")

;; redefine org-agenda-goto
(require 'org-agenda)
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
(defun org-agenda-goto-with-fun (fun &optional highlight)
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

(defvar durand-before-obj nil
  "The position to return to when ivy-read ends")

;;;###autoload
(defun durand-cursor-follow-link ()
  "Place the cursor on the link in ivy-read; inspired by swiper"
  (with-ivy-window
    (swiper--cleanup)
    (let* ((cur (ivy-state-current ivy-last))
           (beg (and cur (cadr (assoc-default cur (caddr durand-before-obj)))))
           (end (and cur (caddr (assoc-default cur (caddr durand-before-obj)))))
           (wnd (ivy-state-window ivy-last)))
      (swiper--add-overlay beg end 'swiper-line-face wnd 0))))

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
                             (let ((choice (ivy-read "Chois un lien à remplaçer: "
                                                     (mapcar (lambda (x)
                                                               (concat (cadr x) " - " (car x)))
                                                             contained-links))))
                               (cl-find choice contained-links
                                        :test (lambda (x y)
                                                (string= x (concat (cadr y) " - " (car y)))))))))
         (all-cands (append contained-links org-stored-links))
         (chosen-link (cond
                       (link (list link))
                       (choose-link
                        (cl-assoc (ivy-read "Chois un lien pour remplaçer: "
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
      (goto-char next-heading-position)
      (newline)
      (forward-char -2)
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
;;;###autoload
  (setq org-note-regexp (concat
			 "^\\([ \t]+\\)- Note taken on \\("
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
            (let ((ending (save-excursion (re-search-forward ":END:" limit t) (point)))
                  res-list)
              (while (re-search-forward
                      (mapconcat #'identity `("state" ,org-ts-regexp3) ".*")
                      ending
                      t)
                (push (list
                       (string-to-number (match-string 2))
                       (string-to-number (match-string 3))
                       (string-to-number (match-string 4))
                       (string-to-number (match-string 7))
                       (string-to-number (match-string 8)))
                      res-list))
              (mapcar (lambda (x)
                        (encode-time 0 (nth 4 x) (nth 3 x) (caddr x) (cadr x) (car x)))
                      res-list)))))))))

;;;###autoload
(defun durand-org-view-notes ()
  "View the notes entries in a separate buffer"
  (interactive)
  (let* ((notes (durand-org-get-notes))
         (logs (durand-org-get-logs))
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
     (org-mode)
     (let ((temp-map (make-sparse-keymap)))
       (set-keymap-parent temp-map org-mode-map)
       (map! :map temp-map
             :n [?q] 'quit-window)
       (use-local-map temp-map)))
    (message "%s note%s found" (if (= 0 len) "No" (number-to-string len))
             (cond ((= len 0) "s") ((<= len 1) "") (t "s")))))

;;;###autoload
(defun durand-org-view-all-logs (&optional match)
  "View all logs recorded in \"aujourdhui.org\" and
\"aujourdhui.org_archive\" matched bt MATCH."
  (interactive)
  (let* ((match (progn
                  (unless (stringp (or match "run"))
                    (user-error "MATCH should be a stirng."))
                  (or match "run")))
         (log-buffer-name (concat "*logs: " match " *"))
         logs)
    (with-current-file "~/org/aujourdhui.org_archive" nil
      (setf logs
            (append logs
                    (apply #'append
                           (org-map-entries #'durand-org-get-logs match)))))
    (with-current-file "~/org/aujourdhui.org" nil
      (setf logs
            (append logs
                    (apply
                     #'append
                     (org-map-entries #'durand-org-get-logs match)))))
    (switch-to-buffer log-buffer-name)
    (durand-draw-calendar-days logs)
    (org-mode)))

;;;###autoload
(defun durand-draw-calendar-days (days-list)
  "Draw days in calendar format.
DAYS-LIST should be a list of time values."
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
                                             (dotimes (i len (nreverse temp))
                                               (let* ((day-num (+ i pointer))
                                                      (day-str
                                                       (if (member day-num special)
                                                           (propertize
                                                            (number-to-string day-num)
                                                            'font-lock-face '(:foreground "gold"))
                                                         (number-to-string day-num))))
                                                 (push (pad-string-to day-str 2 32)
                                                       temp))))
                                     " ")
                        (mapconcat #'identity
                                   (append
                                    (dotimes (i len (nreverse temp))
                                      (let* ((day-num (+ i pointer))
                                             (day-str
                                              (if (member day-num special)
                                                  (propertize
                                                   (number-to-string day-num)
                                                   'font-lock-face '(:foreground "gold"))
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
      (let* ((alpha (pop res-strings))
             (beta (pop res-strings))
             (gamma (pop res-strings))
             (delta (pop res-strings))
             (epsilon (pop res-strings))
             (max-num (max (length alpha) (length beta) (length gamma)
                           (length delta) (length epsilon))))
        (setf alpha (append alpha
                            (make-list (- max-num (length alpha)) (make-string 20 32)))
              beta (append beta
                           (make-list (- max-num (length beta)) (make-string 20 32)))
              gamma (append gamma
                            (make-list (- max-num (length gamma)) (make-string 20 32)))
              delta (append delta
                            (make-list (- max-num (length delta)) (make-string 20 32)))
              epsilon (append epsilon
                              (make-list (- max-num (length epsilon)) (make-string 20 32))))
        (cl-mapcar (lambda (alpha-element
                            beta-element gamma-element
                            delta-element epsilon-element)
                     (insert alpha-element)
                     (insert "  ")
                     (insert beta-element)
                     (insert "  ")
                     (insert gamma-element)
                     (insert "  ")
                     (insert delta-element)
                     (insert "  ")
                     (insert epsilon-element)
                     (insert "\n"))
                   alpha beta gamma delta epsilon)
        (insert "\n")))))

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
                       (concat separator pre-padding day-string post-padding)))
                   (list-days-between starting-date ct))))))
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
  "merge two lists whose elements are strings"
  (let ((la (length a))
        (lb (length b)))
    (cond
     ((< la lb)
      (setf a (append a (make-list (- lb la) ""))))
     ((> la lb)
      (setf b (append b (make-list (- la lb) "")))))
    (cl-mapcar #'concat a b)))

;;;###autoload
(defun durand-org-agenda-goto-view-note ()
  "Go to the corresponding file and view the notes from the agenda file."
  (interactive)
  (org-agenda-goto-with-fun 'durand-org-view-notes)
  (temp-buffer-window-show "*durand-org-view-notes*")
  (save-selected-window
    (pop-to-buffer "*durand-org-view-notes*")
    (goto-char (point-min))
    (fit-window-to-buffer nil temp-buffer-max-height)))

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
          (setf choice (ivy-read "Jump to:" (mapcar #'car items)
                                 :require-match t
                                 :initial-input initial-input
                                 :re-builder 'ivy--regex-plus)))))
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
         (choice (durand-choose-list cands nil "Chois un lien: ")))
    (mapcar
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
         (limite (save-excursion
                   (outline-next-heading)
                   (point)))
         (liste (let (res)
                  (while (re-search-forward org-link-any-re limite t)
                    (push (list (match-string-no-properties 2)
                                (match-string-no-properties 3))
                          res))
                  (cl-remove-if
                   (lambda (ls)
                     (or (null (car ls))
                         (null (cadr ls))))
                   res))))
    (cond
     ((null arg)
      (cons texte liste))
     ((memq arg '(youtube all))
      (list texte liste pt))
     (t
      (cons texte pt)))))

;;;###autoload
(defun durand-choose-list-format-function (cands matched)
  "Format candidates to `durand-choose-list'."
  (ivy--format-function-generic
   (lambda (e)
     (concat (propertize "☸ " 'face 'durand-arrow-face)
             (ivy--add-face e 'ivy-current-match)))
   (lambda (e)
     (if (cl-member e matched :test #'string=)
         (concat (propertize "☸ " 'face 'durand-arrow-face)
                 (ivy--add-face e 'ivy-current-match))
       (concat "   " e)))
   cands "\n"))

;;;###autoload
(defun durand-choose-list (cands &optional all texte non-quick display-cadr)
  "Choose from an alist. Multiple selection is supported.
If ALL is non-nil, add a choice to select all of them.
If NON-QUICK is non-nil, then offer the selection even when there is only one candidate.
If DISPLAY-CADR is non-nil, then display cadr rather than car."
  (if (and (= (length cands) 1) (null non-quick))
      (list (caar cands))
    (let ((cands (if all (cons '("all") cands) cands))
          (question (or texte "Chois un: "))
          res det exc)
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
                              (user-error "durand-choose-list: argument not a list: %s" x))))
                          cands))
      (while (null det)
        (setf det t
              exc nil)
        (let* ((ivy-format-functions-alist
                '((durand-choose-list . (lambda (cands)
                                          (durand-choose-list-format-function cands res)))))
               (ele (ivy-read question cands
                              :require-match t
                              :action '(1
                                        ("o" identity "default")
                                        ("m" (lambda (x)
                                               (setf det nil))
                                         "continue")
                                        ("e" (lambda (x)
                                               (setf det nil
                                                     ivy--index 0
                                                     exc t))
                                         "exclude"))
                              :preselect ivy--index
                              :caller 'durand-choose-list)))
          (unless exc (push ele res))
          (when exc
            (setf cands
                  (cl-remove-if
                   (lambda (y)
                     (string= (if (listp y) (car y) y)
                              (if (stringp ele) ele (car ele))))
                   cands)))))
      (when (member "all" res) (setf res (mapcar #'car (cdr cands))))
      (setf res (cl-remove-duplicates res :test #'string=)
            res (mapcar (lambda (x)
                          (cadr (assoc x cands #'string=)))
                        res))
      res)))

;;;###autoload
(defun org-open-novels (&optional arg)
  "Choose novel to open. By default, filter out qidian links.
With \\[universal-argument], update novels;
With \\[universal-argument]\\[universal-argument], show all links.
With \\[universal-argument]\\[universal-argument]\\[universal-argument], visit every qidian link."
  (interactive)
  (let* ((action (if (memq arg '(open update all qidian))
                     arg
                   (ivy-read "Quel action: "
                             '("open" "update" "open qidian" "open all")
                             :require-match t)))
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
          (mapc #'browse-url liste-de-choix))))
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
          (mapc #'browse-url liste-de-choix))))
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
          (mapc #'browse-url (-filter #'stringp choix-de-liens))))
      (message "Opening novels' qidian pages...DONE")))))

;;;###autoload
(defun org-update-novels (&optional desc)
  "Update the html link to a novel, or to a web_link.
If DESC is non-`nil', then it is the description of the new link."
  (interactive)
  (let* ((tags (ivy-read "tag: " '("roman-ARCHIVE"
                                   "web_link-ARCHIVE")
                         :require-match t))
         (roman-p (string-match "roman" tags))
         (prompt (if roman-p
                     "Chois un roman à mettre à jour: "
                   "Chois un web lien à mettre à jour: "))
         cands)
    (with-current-file "/Users/durand/org/notes.org" nil
      (setf cands (org-map-entries
                   (lambda () (durand-org-link-info t))
                   tags))
      (unless roman-p (setf cands (nreverse cands)))
      (let* ((choix (ivy-read prompt cands :require-match t))
             (item (cl-assoc choix cands :test #'string=))
             (lien (read-string "Le lien: " (current-kill 0 t)))
             (desc (if roman-p lien desc)))
        (goto-char (cdr item))
        (org-update-link lien nil nil desc)))))

;;;###autoload
(defun org-open-youtube (&optional arg)
  "Choose youtube link to open.
With \\[universal-argument], just kill the entry.
With \\[universal-argument] \\[universal-argument], don't kill the entry."
  (interactive "P")
  (cond
   ((or (null arg) (equal arg '(16)))
    (let* (cands)
      (with-current-file "/Users/durand/org/notes.org" nil
        (setf cands (org-map-entries
                     (lambda () (durand-org-link-info 'youtube))
                     "youtube")))
      (let* ((sel (durand-choose-list cands t "Chois une vidéo: " t))
             (download-or-not (y-or-n-p "Download or not?")))
        (setf sel (sort sel (lambda (x y)
                              (< (cadr (assoc-default x cands))
                                 (cadr (assoc-default y cands))))))
        (with-current-file "/Users/durand/org/notes.org" nil
          (dolist (x (nreverse sel))
            (goto-char (cadr (assoc-default x cands)))
            (unless arg (org-cut-subtree))
            (dolist (y (car (assoc-default x cands)))
              (if download-or-not
                  (durand-download-youtube
                   (car y)
                   (concat x (durand-take-param (car y))))
                (browse-url (car y)))))))))
   (t
    (org-kill-youtube))))

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
  (org-link-open-from-string
   (org-link-make-string
    (org-link-decode (if (stringp str)
                         str
                       (car str)))
    "fake link")))

;; Escape bracket chars as well
;; (add-to-list org-link-escape-chars 20)


;; the original open link function is broken over links with spaces, and here is a fix.
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
	      (let ((type (org-element-property :type context))
              ;; NOTE: I changed this part.
	            (path (org-link-decode (org-element-property :path context))))
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
           cands)
      (with-current-file "/Users/durand/org/notes.org" nil
        (setf cands (org-map-entries #'durand-org-link-info tag)
              cands (nreverse (mapcar (lambda (x) (cons (durand-org-filter-dates (car x)) (cdr x)))
                                      cands))))
      (let ((liste-de-choix
             (let (temp)
               (let* ((sel (durand-choose-list cands nil "Chois un article: ")))
                 (mapc (lambda (x)
                         (setf temp (append temp
                                            (durand-choose-list
                                             (mapcar (lambda (x) (cons x '("1"))) (assoc-default x cands))
                                             t "Chois un lien: "))))
                       sel)
                 temp))))
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
  (let* ((tag (if all "math-a_voir-ARCHIVE" "TODO=\"TO-THINK\"-ARCHIVE")) cands)
    (with-current-file "/Users/durand/org/notes.org" nil
      (setf cands (org-map-entries
                   (lambda () (durand-org-link-info t))
                   tag)
            cands (mapcar (lambda (x)
                            (cons (durand-org-filter-dates (car x)) (cdr x)))
                          cands)
            cands (reverse cands)))
    (let* ((choix (ivy-read "Chois un lien à mettre à jour: " cands
                            :require-match t))
           (item (cl-assoc choix cands :test #'equal)))
      (with-current-file "/Users/durand/org/notes.org" nil
        (goto-char (cdr item))
        (org-update-link nil t)))))

;;;###autoload
(defun org-open-weblink (&optional arg)
  "Open all weblink, that is, entries in \"notes.org\" with \"web_link\" tag.
If ARG is '(4), then execute `durand-update-weblink'.
If ARG is '(16), then execute `durand-mark-weblink'"
  (interactive "P")
  (cond
   ((null arg)
    (let* ((tag "web_link-special-personnes-ARCHIVE") cands)
      (with-current-file "/Users/durand/org/notes.org" nil
        (setf cands (org-map-entries #'durand-org-link-info tag)))
      (setf cands
            (mapcar (lambda (x) (cons (durand-org-filter-dates (car x)) (cdr x))) cands))
      (setf cands (reverse cands))
      (let ((liste-de-choix
             (let (temp)
               (let* ((sel (durand-choose-list cands nil "Chois un lien de web: ")))
                 (dolist (x sel temp)
                   (setf temp (append temp
                                      (durand-choose-list
                                       (assoc-default x cands)
                                       t "Chois un lien: " nil t))))))))
        (mapc #'durand-org-open-link liste-de-choix)
        (delete-other-windows))))
   ((equal arg '(4))
    (durand-update-weblink))
   ((equal arg '(16))
    (durand-mark-weblink))
   (t
    (message "This ARG is not supported: %s" arg))))

;;;###autoload
(defun durand-update-weblink ()
  "Update the link to a weblink;
the link comes from the most recently stored link, so choose carefully the target to update."
  (interactive)
  (let* ((tag "web_link-special-personnes-ARCHIVE") cands)
    (with-current-file "/Users/durand/org/notes.org" nil
      (setf cands (org-map-entries
                   (lambda () (durand-org-link-info t))
                   tag))
      (setf cands (mapcar (lambda (x)
                            (cons (durand-org-filter-dates (car x)) (cdr x)))
                          cands))
      (setf cands (reverse cands)))
    (let* ((choix (ivy-read "Chois un lien à mettre à jour: " cands
                            :require-match t))
           (item (cl-assoc choix cands :test #'string=))
           (link (plist-get org-store-link-plist :link))
           (desc (file-name-nondirectory
                  (plist-get org-store-link-plist :description))))
      (with-current-file "/Users/durand/org/notes.org" nil
        (goto-char (cdr item))
        (org-update-link link nil nil desc)))))

;;;###autoload
(defun durand-mark-weblink ()
  "Choose web links and execute `durand-mark-links' for each of them."
  (interactive)
  (with-current-file "/Users/durand/org/notes.org" nil
    (let* ((tag "web_link-special-personnes-ARCHIVE")
           (cands (org-map-entries (lambda () (durand-org-link-info t)) tag))
           (cands (mapcar (lambda (x)
                            (cons (durand-org-filter-dates (car x))
                                  (cdr x)))
                          cands))
           (cands (nreverse cands))
           (choice-headings (durand-choose-list cands t "Chois un lien à marquer: " t))
           (choice-elements (mapcar (lambda (x)
                                      (cl-assoc x cands :test #'string=))
                                    choice-headings)))
      (cl-loop for element in choice-elements
               do (save-excursion
                    (goto-char (cdr element))
                    (durand-mark-links))))))

;;;###autoload
(defun durand-mark-links ()
  "Mark the links in a heading.
marquer: mark as seen.
presque marquer: mark as mostly seen.
unmarquer: remove the seen and the mostly seen mark.
tuer: delete the link."
  (interactive)
  (let* ((next-heading-position (save-excursion
                                  (outline-next-heading)
                                  (point)))
         (contained-links (save-excursion
                            (nreverse
                             (cl-loop while (re-search-forward org-link-any-re next-heading-position t)
                                      collect (list (match-string-no-properties 2)
                                                    (match-string-no-properties 3)
                                                    (match-beginning 0)
                                                    (match-end 0))))))
         (chosen-links (durand-choose-list contained-links t "Chois un lien: " t t))
         (action-list (list "marquer" "presque marquer" "unmarquer" "tuer"))
         (chosen-elements (cl-loop for link in chosen-links
                                   collect (cl-assoc link contained-links :test #'string=)))
         (chosen-action (ivy-read "Quoi faire? " action-list :require-match t)))
    (cond
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
         (choice (ivy-read "Which note to modify? " note-choices
                           :require-match t
                           :caller 'durand-org-modify-note))
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
         (choice (ivy-read "Which note to delete? " note-choices
                           :require-match t
                           :caller 'durand-org-delete-note))
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
                                    (ivy-read "Choose a day to update: " (mapcar #'car candidates)
                                              :require-match t
                                              :caller 'org-update-account)
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
          (target-entry (ivy-read "Choose one entry: "
                                  (mapcar #'car account-field-list)
                                  :require-match t
                                  :caller 'org-modify-account))
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
          (target-entry (ivy-read "Choose one entry: "
                                  (mapcar #'car account-field-list)
                                  :require-match t
                                  :caller 'org-modify-account))
          (target-position (caddr (assoc target-entry account-field-list))))
     (goto-char target-position)
     (skip-chars-forward "* ")
     (org-mark-subtree)
     (delete-region (region-beginning) (region-end)))))

;;;###autoload
(defun org-set-account-according-to-date (date &optional month year)
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
  (interactive (let ((item (ivy-read "Enter item: "
                                     '("breakfast" "brunch" "brunverage"
                                       "lunch" "dinner" "beverage")
                                     :caller 'org-set-item-price-note))
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
(defun org-delete-item-price-note (row-num &optional total-num)
  (interactive (let* ((total-num (save-excursion
                                   (goto-char (point-max))
                                   (outline-show-all)
                                   (re-search-backward "tblfm")
                                   (forward-line -2)
                                   (org-table-current-line)))
                      (num (ivy-read "Enter row number: "
                                     (mapcar #'number-to-string (number-sequence 1 total-num))
                                     :caller 'org-delete-item-price-note)))
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
  (interactive (list (ivy-read "DAY: "
                               (if current-prefix-arg
                                   (org-find-all-days)
                                 (save-restriction
                                   (widen)
                                   (org-find-all-days)))
                               :re-builder 'ivy--regex-ignore-order)
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
  "Use `ivy-read' to choose a key."
  (interactive)
  (let* ((temps (org-capture-upgrade-templates org-capture-templates))
         (choix (mapcar (lambda (x)
                          (cons (string-join (list (car x)
                                                   (cadr x))
                                             ": ")
                                (car x)))
                        temps))
         (clé (assoc-default (ivy-read "Chois un clé: " choix
                                       :require-match t
                                       :caller 'durand-capture
                                       :update-fn 'durand-self-insert-complete-and-exit
                                       :unwind 'reset-durand-changed
                                       :initial-input "^")
                             choix)))
    (org-capture nil clé)))

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
