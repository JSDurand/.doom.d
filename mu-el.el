;; elfeed
;;;###autoload
(defun durand-lighten-name (name percent &optional digit)
  "Lighten color NAME by PERCENT.
     The defualt function gives four digits spec, not useful.
     So you can specify digits per component by DIGIT."
  (let ((digit (or digit 2)))
    (apply 'color-rgb-to-hex
	   (append
	    (apply 'color-hsl-to-rgb
		   (apply 'color-lighten-hsl
			  (append
			   (apply 'color-rgb-to-hsl
				  (color-name-to-rgb name))
			   (list percent))))
	    (list digit)))))

(use-package! elfeed
  :ensure t
  :config
  (define-key ctl-x-map [?w] #'elfeed))

;; Sometimes elfeed crashes emacs, and I cannot find the reason.
(setq elfeed-feeds
      '(;; ("http://nullprogram.com/feed/"
	;;  program)
	;; ("http://planet.emacsen.org/atom.xml"
	;;  emacs
	;;  blog)
	("https://lukesmith.xyz/rss.xml"
	 luke
	 blog)
	("http://notrelated.libsyn.com/rss"
	 luke
	 relevant
	 podcast)
	;; ("https://stackexchange.com/feeds/tagsets/347224/favorite-tags?sort=active"
	;;  stackexchange
	;;  favorite)
	;; ("https://stackexchange.com/feeds/tagsets/347226/real-love?sort=active"
	;;  real-love
	;;  interests)
	("https://www.reddit.com/r/emacs/.rss"
	 emacs
	 reddit)
	("https://math.stackexchange.com/feeds/tag?tagnames=number-theory&sort=newest"
	 interests
	 favorite)
	("https://mattbaker.blog/feed/"
	 interests
	 mattbaker
	 blog)
	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTfRwznpxtbjQQQJ_15Fk2w"
	;;  youtube
	;;  3M)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"
	 youtube
	 3blue1brown)
	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyC_4jvPzLiSkJkLIkA7B8g"
	;;  youtube
	;;  music
	;;  lindsey)
	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCPRWWKG0VkBA0Pqa4Jr5j0Q"
	;;  youtube
	;;  joeman)
	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCjhwHd3mgmqm0ONm0bXKmng"
	;;  youtube
	;;  anju)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCcXhhVwCT6_WqjkEniejRJQ"
	 wintergarten
	 youtube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtR5okwgTMghi_uyWvbloEg"
         lao_kao
         youtube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCI4xp8qHD1MDErkqxb1dPbA"
         innerFrench
         youtube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTx8xR4VZ46IAsp5yBgLMzw"
         endClashRoyale
         youtube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8TtAsZE51ekqffnNASo7DA"
         dacon
         youtube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMUnInmOkrWN4gof9KlhNmQ"
         KaoetMuo
         youtube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCwYJs4-yKmaALkLFX1uHFsw"
         drama
         youtube)
	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqripRcC8scod22F5NKvLcQ"
	;;  julia
	;;  youtube)
	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKSiE8dIEWsT-1jQCGrOqtw"
	;;  youtube
	;;  little-white)
	;; ("https://math.stackexchange.com/feeds/question/2883754"
	;;  math
	;;  relation
	;;  important)
	;; ("https://haskellweekly.news/haskell-weekly.atom"
	;;  haskell
	;;  relevant
	;;  blog)
	("https://themonadreader.wordpress.com/feed/"
	 monad-reader
	 blog
	 important)
	;; ("https://www.reddit.com/r/haskell/.rss"
	;;  haskell
	;;  reddit)
	;; ("https://www.archlinux.org/feeds/news/"
	;;  archlinux
	;;  relevant)
	))
(with-eval-after-load 'elfeed
  (add-to-list 'elfeed-search-face-alist
	       '(emacs elfeed-emacs-face))
  (add-to-list 'elfeed-search-face-alist
	       '(relevant elfeed-relevant-face))
  (add-to-list 'elfeed-search-face-alist
	       '(luke elfeed-relevant-face))
  (add-to-list 'elfeed-search-face-alist
	       '(important elfeed-math-face))
  (add-to-list 'elfeed-search-face-alist
	       '(youtube elfeed-youtube-face))
  ;; my primary interests
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :feed-url "math.*stackexchange"
				:feed-title "number\\|class\\|algebraic\\|field\\|elliptic\\|cohomology\\|group"
				:add '(math important)))
  ;; don't mark old ones as unread
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "1 week ago"
				:remove 'unread))
  ;; my secondary interests
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :feed-url "math.*stackexchange"
				:feed-title "geometry\\|topology\\|hodge\\|graph"
				:add '(math relevant)))
  (define-key elfeed-search-mode-map [?i] 'important-tag-toggler)
  (define-key elfeed-search-mode-map [?l] 'relevant-tag-toggler)
  (define-key elfeed-search-mode-map [?e] 'emacs-tag-toggler)
  (define-key elfeed-search-mode-map [?m] 'math-toggler)
  (define-key elfeed-search-mode-map [?U] 'unread-tag-toggler)
  (define-key elfeed-search-mode-map [?y] 'youtube-tag-toggler)
  (define-key elfeed-search-mode-map [?h] 'haskell-tag-toggler)
  (define-key elfeed-search-mode-map [?b] 'elfeed-visit-or-play-with-mpv)
  (define-key elfeed-search-mode-map [?d] 'elfeed-download-youtube)
  (define-key elfeed-search-mode-map [?n] #'elfeed-next-entry)
  (define-key elfeed-search-mode-map [?p] #'elfeed-previous-entry)
  (define-key elfeed-show-mode-map [?b] 'elfeed-visit-or-play-with-mpv))


;;;###autoload
(defun elfeed-next-entry (&optional arg)
  "Go to the next entry in elfeed search buffer."
  (interactive "p")
  (when (looking-at "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
    (forward-char))
  (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t arg)
  (beginning-of-visual-line))

;;;###autoload
(defun elfeed-previous-entry (&optional arg)
  "Go to the previous entry in elfeed search buffer."
  (interactive "p")
  (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t (- arg))
  (beginning-of-visual-line))

(setq-default elfeed-search-filter "@1week-ago +unread")

(defface elfeed-math-face
  `((t . (:background "gray10" :foreground "deep sky blue")))
  "face for math feed")

(defface elfeed-relevant-face
  `((t . (:background "gray10" :foreground "light blue")))
  "face for relevant feed")

(defface elfeed-emacs-face
  `((t . (:background "gray10" :foreground "orange")))
  "face for relevant feed")
(defface elfeed-youtube-face
  `((t . (:background "gray10" :foreground "yellow")))
  "face for youtube feed")

;; play youtube video
;;;###autoload
(defun durand-play-with-mpv (quality url)
  " Play a given URL with mpv."
  (interactive (list (durand-get-quality-val)
		     (read-string "Enter URL: ")))
  (let ((quality-arg "")
	(quality-val quality)
	(fit-arg "--autofit=100%x100%"))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height ≤ %s with mpv..."
	     url
	     quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]"
				quality-val)))
    (eshell)
    (insert (format "mpv --no-terminal %s %s %s &"
		    quality-arg
		    fit-arg
		    url))
    (eshell-send-input)))

;;;###autoload
(defun durand-play-with-mpv-in-elfeed (quality)
  "If currently visiting a youtube feed entry or if the cursor is on a youtube feed entry,
then play the video with mpv with QUALITY, else just inform this is not a youtube link."
  (interactive (list (durand-get-quality-val)))
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
		   elfeed-show-entry
		 (elfeed-search-selected t)))
	(quality-arg "")
	(quality-val quality)
	(fit-arg "--autofit=100%x100%"))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height ≤ %s with mpv..."
	     (elfeed-entry-link entry)
	     quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]"
				quality-val)))
    ;; (start-process "elfeed-mpv" nil "mpv" quality-arg fit-arg (elfeed-entry-link entry))
    (eshell)
    (insert (format "mpv --no-terminal %s %s %s &"
		    quality-arg
		    fit-arg
		    (elfeed-entry-link entry)))
    (eshell-send-input)))

(defvar elfeed-mpv-patterns
  '("youtu\\.?be")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.
Default value is \"youtu\\.?be\"")

;;;###autoload
(defun elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns'; visit it otherwise.
See `durand-play-with-mpv' also."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
		   elfeed-show-entry
		 (elfeed-search-selected t)))
	(patterns elfeed-mpv-patterns))
    (while (and patterns
		(not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
	(call-interactively 'durand-play-with-mpv-in-elfeed)
      (if (eq major-mode 'elfeed-search-mode)
	  (elfeed-search-browse-url)
	(elfeed-show-visit)))))

;;;###autoload
(defun durand-download-youtube (url &optional title)
  "
Download the URL with youtube-dl"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'eshell))
  (insert "cd ~/Desktop/Centre/Vidéos")
  (eshell-send-input)
  (insert (if (not title)
              (format "youtube-dl -f 22 %s" url)
            (format "youtube-dl -f 22 %s -o \"%s.mp4\"" url title)))
  (eshell-send-input)
  (if title
      (message "Downloading %s as %s.mp4" url title)
    (message "Downloading %s" url)))

;;;###autoload
(defun elfeed-download-youtube ()
  "
Play in mpv if entry link matches `elfeed-mpv-patterns'; do nothing otherwise."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
		    elfeed-show-entry
		  (elfeed-search-selected t)))
	 (link (elfeed-entry-link entry))
         (title (elfeed-entry-title entry))
	 (patterns elfeed-mpv-patterns))
    (while (and patterns
		(not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (when patterns
      (durand-download-youtube link title))))

;;;###autoload
(defun durand-get-quality-val ()
  "Let the user choose a quality format."
  (ivy-read "Max height resolution (0 for unlimited): "
	    '("0" "480" "720")
	    :caller 'durand-get-quality-val))

;;;###autoload
(defmacro make-tag-toggler (arg)
  "
Make a function that toggles the tag ARG on or off in elfeed search"
  `(defun ,(intern (format "%s-tag-toggler" arg)) ()
     ,(format "Toggle %s tag" arg)
     (interactive)
     (cond ((string-match (concat " \\+" ,arg) elfeed-search-filter)
	    (elfeed-search-set-filter (replace-match "" nil nil elfeed-search-filter)))
	   ((string-match (concat " -" ,arg) elfeed-search-filter)
	    (elfeed-search-set-filter (replace-match (concat " +" ,arg) nil nil elfeed-search-filter)))
	   (t
	    (elfeed-search-set-filter (concat elfeed-search-filter (concat " -" ,arg)))))
     (message elfeed-search-filter)))

;;;###autoload
(defmacro make-toggler (arg)
  "
Make a function that toggles ARG on or off in elfeed search"
  `(defun ,(intern (format "%s-toggler" arg)) ()
     ,(format "Toggle %s" arg)
     (interactive)
     (cond ((string-match (concat " " ,arg) elfeed-search-filter)
	    (elfeed-search-set-filter (replace-match "" nil nil elfeed-search-filter)))
	   (t
	    (elfeed-search-set-filter (concat elfeed-search-filter (concat " " ,arg)))))
     (message elfeed-search-filter)))

(make-tag-toggler "youtube")
(make-tag-toggler "emacs")
(make-tag-toggler "blog")
(make-tag-toggler "relevant")
(make-tag-toggler "important")
(make-tag-toggler "unread")
(make-tag-toggler "haskell")
(make-toggler "math")

;; The code for clearing up data-base
;; when .elfeed gets over 20M, then I would like to clear that directory and start over.
;; (with-elfeed-db-visit (entry _)
;;   (setf (elfeed-entry-content entry) nil))

;; (elfeed-db-gc)

;; (defvar durand-pattern-alist '(("math" . 1)
;; 			       ("number.*theory" . 2)
;; 			       ("elliptic" . 2)
;; 			       ("cyclotomic" . 2)
;; 			       ("cohomology" . 2))
;;   "Patterns to match against an elfeed entry in auto-tagging, along with an attached score.")

;; mu4e
(add-to-list 'load-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e/")
(require 'mu4e)
(define-key mu4e-main-mode-map (vector ?u) 'mu4e-update-mail-and-index)
;; (define-key mu4e-main-mode-map (vector ?q) 'bury-buffer)
(map! :map mu4e-main-mode-map :n [?q] 'bury-buffer)
;; Je ne veux pas quitter mu4e quand je touche "q".

(setf mu4e-confirm-quit nil)
(setq mu4e-maildir (expand-file-name "~/mbsync"))
(setq mu4e-get-mail-command "mbsync gmail") ; mbsync works a lot better!
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-view-show-addresses t)	; show full addresses!
(setq mu4e-view-show-images t)
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-use-fancy-chars t)
;; (setq mu4e-update-interval nil)
(setq mu4e-completing-read-function 'ivy-completing-read)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
;; (setq mu4e-maildir-shortcuts
;;       '(("/INBOX" . ?i)
;; 	("/archive" . ?a)))

(defun mu4e-view-attach-emacs (n)
  "Open the N-th attachment in emacs"
  (interactive (list (read-number "Enter attachment number: " 1)))
  (mu4e-view-open-attachment-emacs (mu4e-message-at-point) n))

(add-to-list
 'mu4e-bookmarks
 (make-mu4e-bookmark
  :name "week no trash no archive"
  :query "date:7d..now AND NOT maildir:/trash AND NOT maildir:/archive"
  :key ?d))

(add-to-list
 'mu4e-bookmarks
 (make-mu4e-bookmark
  :name "week starred"
  :query "date:7d..now AND maildir:/suivis"
  :key ?s))

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/archive" . ?a)
        ("/suivis" . ?s)
        ("/drafts" . ?d)))

(define-key mu4e-view-mode-map [?o] 'mu4e-view-attach-emacs)
(define-key mu4e-view-mode-map (kbd "<home>") 'general-hydra/body)

(add-to-list 'mu4e-view-actions
	     '("Browse this mail" . mu4e-action-view-in-browser))

(add-to-list 'mu4e-view-actions
	     '("view attachment in pdf-view" . mu4e-view-attach-emacs))

(remove '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions)


(setq mu4e-contexts
      `(,(make-mu4e-context
	  :name "Awllower"
	  :enter-func (lambda () (mu4e-message "Entering Awllower context"))
	  :leave-func (lambda () (mu4e-message "Leaving Awllower context"))
	  ;; we match based on the contact-fields of the message
	  :match-func (lambda (msg)
			(when msg
			  (or
			   (mu4e-message-contact-field-matches msg :to "mmemmew@gmail.com")
			   (mu4e-message-contact-field-matches msg :from "mmemmew@gmail.com"))))
	  :vars '( ( user-mail-address	    . "mmemmew@gmail.com"  )
		   ( user-full-name	    . "李俊緯" )
		   ( mu4e-compose-signature . "#+BEGIN_EXPORT html
<span style=\"color:rgb(6,144,255)\">生 俊緯</span>
#+END_EXPORT")
		   (mu4e-sent-folder . "/gmail/sent")
		   (smtpmail-smtp-user . "mmemmew")
		   (smtpmail-local-domain . "gmail.com")
		   (smtpmail-default-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-service . 587)))
	,(make-mu4e-context
	  :name "NCTS"
	  :enter-func (lambda () (mu4e-message "Switch to the NCTS context"))
	  :leave-func (lambda () (mu4e-message "Leave NCTS context"))
	  ;; no leave-func
	  ;; we match based on the maildir of the message
	  ;; this matches maildir /Arkham and its sub-directories
	  :match-func (lambda (msg)
			(when msg
			  (or
			   (mu4e-message-contact-field-matches msg :to "chunweilee@ncts.ntu.edu.tw")
			   (mu4e-message-contact-field-matches msg :from "chunweilee@ncts.ntu.edu.tw"))))
	  :vars '( ( user-mail-address	     . "chunweilee@ncts.ntu.edu.tw" )
		   ( user-full-name	     . "李俊緯" )
		   ( mu4e-compose-signature  .
					     (concat
					      "Sincerely Yours,\n"
					      "俊緯"))))
	,(make-mu4e-context
	  :name "BaoBao"
	  :enter-func (lambda () (mu4e-message "Entering 寶寶 context"))
	  :leave-func (lambda () (mu4e-message "Leaving 寶寶 context"))
	  ;; we match based on the contact-fields of the message
	  :match-func (lambda (msg)
			(when msg
			  (or
			   (mu4e-message-contact-field-matches msg :to "lintingtsen@gmail.com")
			   (mu4e-message-contact-field-matches msg :from "lintingtsen@gmail.com"))))
	  :vars '( ( user-mail-address	    . "mmemmew@gmail.com"  )
		   ( user-full-name	    . "大寶寶" )
		   ( mu4e-compose-signature .
					    "愛你的寶寶")
		   (mu4e-sent-folder . "/gmail/sent")
		   (smtpmail-smtp-user . "mmemmew")
		   (smtpmail-local-domain . "gmail.com")
		   (smtpmail-default-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-service . 587)))))

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)
(setf mu4e-attachment-dir "~/Downloads")

;; compose with the current context is no context matches;
;; default is to ask
;; (setq mu4e-compose-context-policy nil)

;; mu4e-alert
;; it makes emacs very slow...
;; But per chance some functionalities of mu4e depend upon it. So
;; don't hurry to delete this package.
(setf mu4e-mu-binary "/usr/local/bin/mu")
(use-package! mu4e-alert
  :ensure t
  :after mu4e
  :config
  (setf mu4e-alert-interesting-mail-query
        "date:7d..now AND NOT maildir:/trash AND NOT maildir:/archive AND flag:unread"
        mu4e-update-interval 3600)
  
  (mu4e-alert-enable-mode-line-display)

  ;; do what I mean
  (defun durand-mu4e (&optional arg)
    "If there are unread mails, view them; else, show the time until the next update,
unless called multiple times, in which case execute `mu4e'.
With ARG, toggle mu4e.
If mu4e is not turned on, then tell the user this fact."
    (interactive "P")
    (cond
     ((and arg (or (get-process " *mu4e-proc*")
                   mu4e~update-timer))
      (mu4e-quit)
      (message "mu4e is turned off now."))
     (arg
      (mu4e))
     ((> mu4e-alert-mode-line 0) (mu4e-alert-view-unread-mails))
     ;; ((eq last-command 'durand-mu4e) (mu4e))
     ((and (memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
           mu4e~update-timer)
      (mu4e)
      (mu4e-next-update-seconds))
     ((memq last-command `(durand-mu4e ,(intern "general-hydra/lambda-,m")))
      (mu4e)
      (setq mu4e~update-timer
            (run-at-time
             0 mu4e-update-interval
             (lambda () (mu4e-update-mail-and-index
                         mu4e-index-update-in-background)))))
     ((and (get-process " *mu4e-proc*")
           mu4e~update-timer)
      (mu4e-next-update-seconds))
     ((or (and (not (get-process " *mu4e-proc*"))
               mu4e~update-timer)
          (and (get-process " *mu4e-proc*")
               (null mu4e~update-timer)))
      (mu4e-quit)
      (user-error "Fixé une erreur."))
     (t
      (message "mu4e is not active right now."))))
  
  (defun durand-mu4e-format-string (&rest args)
    "Format string"
    (interactive)
    (let* ((args (-filter (lambda (ele) (/= (car ele) 0)) args))
           (derniere (last args))
           (except-last (reverse (cdr (reverse args))))
           (number (caar derniere))
           (str (cadar derniere))
           (number-face '(:foreground "gold"))
           (str-face '(:foreground "red"))
           res)
      (dolist (var except-last)
        (when (listp var)
          (let ((number (car var))
                (str (cadr var)))
            (setf res (concat
                       res
                       (format " %s %s"
                               (propertize (format "%d" number) 'face number-face)
                               (propertize (if (> number 1) (concat str "s") str)
                                           'face str-face))
                       (if (>= (length args) 3) "," ""))))))
      (setf res (concat
                 (when res (substring res 1))
                 (if (<= (length args) 1)
                     ""
                   " et ")
                 (format "%s %s avant le prochain mis-à-jour."
                         (propertize (format "%d" number) 'face number-face)
                         (propertize (if (> number 1) (concat str "s") str)
                                     'face str-face))))))

  (defun mu4e-next-update-seconds ()
    "Return the number of seconds before the next automatic update"
    (interactive)
    (let* ((time (time-subtract `(,(aref mu4e~update-timer 1) ,(aref mu4e~update-timer 2)
                                  ,(aref mu4e~update-timer 3) ,(aref mu4e~update-timer 4))
                                (current-time)))
           (total-secs (truncate (time-to-seconds time)))
           (days (/ total-secs 86400))
           (hours (% (/ total-secs 3600) 24))
           (mins (% (/ total-secs 60) 60))
           (secs (% total-secs 60))
           (str (durand-mu4e-format-string `(,days "jour")
                                           `(,hours "heure")
                                           `(,mins "min")
                                           `(,secs "sec")))
           (update-info (if (get-process "mu4e-update") "Updating, " "")))
      (mu4e-message "%s%s" update-info str)))

  (defun mu4e-update-show-time (oldfun &rest info)
    (if (and
         (eq (plist-get (car info) :info) 'index)
         (not (eq (plist-get (car info) :status) 'running)))
        (progn
          (mu4e-index-message
           "Time: %s; Indexing completed; processed %d, updated %d, cleaned-up %d"
           (format-time-string "%k:%M:%S")
           (plist-get (car info) :processed) (plist-get (car info) :updated)
           (plist-get (car info) :cleaned-up))
          (unless (or (zerop (plist-get (car info) :updated)) (not mu4e~contacts))
            (mu4e~request-contacts-maybe)
            (run-hooks 'mu4e-index-updated-hook)))
      (apply oldfun info)))

  (defun mu4e-contacts-show-time (&rest args)
    "show time"
    (mu4e-index-message "Temps: %s, contacts: %d"
                        (format-time-string "%k:%M:%S")
                        (hash-table-count mu4e~contacts)))

  (advice-add 'mu4e-info-handler :around 'mu4e-update-show-time)
  (advice-add 'mu4e~fill-contacts :after 'mu4e-contacts-show-time)
  
  (global-set-key [?\C-x ?m] 'durand-mu4e)

  (define-key global-map (vector ?\C-x ?M) 'mu4e)

  ;; redefine this search function
  (defun mu4e~headers-search-execute (expr ignore-history)
    "Search in the mu database for EXPR, and switch to the output
buffer for the results. If IGNORE-HISTORY is true, do *not* update
the query history stack."
    ;; note: we don't want to update the history if this query comes from
    ;; `mu4e~headers-query-next' or `mu4e~headers-query-prev'.
    ;;(mu4e-hide-other-mu4e-buffers)
    (let* ((buf (get-buffer-create mu4e~headers-buffer-name))
           (inhibit-read-only t)
           (rewritten-expr (funcall mu4e-query-rewrite-function expr))
           (maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit)))
      (with-current-buffer buf
        (mu4e-headers-mode)
        (unless ignore-history
          ;; save the old present query to the history list
          (when mu4e~headers-last-query
            (mu4e~headers-push-query mu4e~headers-last-query 'past)))
        (setq
         ;; don't set mode-name, s'il vous plaît
         ;; mode-name "mu4e-headers"
         mu4e~headers-last-query rewritten-expr)
        (add-to-list 'global-mode-string
                     '(:eval
                       (concat
                        (propertize
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         'face 'mu4e-modeline-face)
                        " "
                        (mu4e-context-label)
                        (if (and mu4e-display-update-status-in-modeline
                                 (buffer-live-p mu4e~update-buffer)
                                 (process-live-p (get-buffer-process
                                                  mu4e~update-buffer)))
                            (propertize " (updating)" 'face 'mu4e-modeline-face)
                          "")))))

      ;; when the buffer is already visible, select it; otherwise,
      ;; switch to it.
      (unless (get-buffer-window buf 0)
        (switch-to-buffer buf))
      (run-hook-with-args 'mu4e-headers-search-hook expr)
      (mu4e~headers-clear mu4e~searching)
      (mu4e~proc-find
       rewritten-expr
       mu4e-headers-show-threads
       mu4e-headers-sort-field
       mu4e-headers-sort-direction
       maxnum
       mu4e-headers-skip-duplicates
       mu4e-headers-include-related))))

;; two timers

;;;###autoload
(defun durand-mu4e-open-if-necessary ()
  "If `mu4e' is not already open, then open it."
  (cond
   ((get-process " *mu4e-proc*")
    (message "déjà ouvert"))
   (t
    (mu4e)
    (switch-to-buffer (other-buffer))
    (message "ouvert maintenant"))))

;;;###autoload
(defun durand-mu4e-close-if-necessary ()
  "If `mu4e' is open, then close it."
  (cond
   ((get-process " *mu4e-proc*")
    (mu4e-quit)
    (message "fermé maintenant"))
   (t
    (message "déjà fermé"))))


(set-evil-initial-state!
  '(mu4e-main-mode
    mu4e-view-mode
    mu4e-compose-mode
    mu4e~update-mail-mode)
  'normal)

(set-evil-initial-state!
  '(mu4e-headers-mode)
  'emacs)

;; advice update function to log something.
(defun mu4e-update-advice (run-in-background)
  "Log it after the update."
  (if run-in-background
      (message "Update in background finished.")
    (message "Update finished.")))

(advice-add 'mu4e-update-mail-and-index :after 'mu4e-update-advice)


;; doom for some weird reason does not want to delete mails, but I do.

(setf mu4e-marks
      (push
       '(delete
         :char ("D" . "x")
         :prompt "Delete"
         :show-target (lambda (target) "delete")
         :action (lambda (docid msg target) (mu4e~proc-remove docid)))
       mu4e-marks))

;;;###autoload
(setf durand-mu4e-open-timer
      (unless (boundp 'durand-mu4e-open-timer)
        (let* ((cur (decode-time (current-time)))
               (cur-year (nth 5 cur))
               (cur-month (nth 4 cur))
               (cur-day (nth 3 cur))
               (cur-hour (nth 2 cur)))
          (run-with-timer
           (float-time
            (time-subtract
             (cond
              ((>= cur-hour 9)
               (encode-time 0 0 9 (1+ cur-day) cur-month cur-year))
              (t
               (encode-time 0 0 9 cur-day cur-month cur-year)))
             nil))
           (* 24 60 60) ;; a day
           #'durand-mu4e-open-if-necessary)))
      durand-mu4e-close-timer
      (unless (boundp 'durand-mu4e-close-timer)
        (let* ((cur (decode-time (current-time)))
               (cur-year (nth 5 cur))
               (cur-month (nth 4 cur))
               (cur-day (nth 3 cur))
               (cur-hour (nth 2 cur)))
          (run-with-timer
           (float-time
            (time-subtract
             (cond
              ((>= cur-hour 22)
               (encode-time 0 0 22 (1+ cur-day) cur-month cur-year))
              (t
               (encode-time 0 0 22 cur-day cur-month cur-year)))
             nil))
           (* 24 60 60) ;; a day
           #'durand-mu4e-close-if-necessary))))
