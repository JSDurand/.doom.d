;;(add-to-list 'load-path (expand-file-name "evil" user-emacs-directory))

(require 'evil)

(evil-mode 1)


;; evil-surround

;; load evil-surround before others in order to avoid conflicts

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1)
;;   (add-hook 'emacs-lisp-mode-hook
;;    (lambda ()
;;      (push '(?` . ("`" . "'"))
;;            evil-surround-pairs-alist))))

(setf evil-normal-state-cursor   'box
      evil-emacs-state-cursor    '(bar . 7)
      evil-insert-state-cursor   'bar
      evil-visual-state-cursor   'bar
      evil-motion-state-cursor   'bar
      evil-replace-state-cursor  'hbar
      evil-operator-state-cursor 'hbar)

(setf evil-cross-lines t)
(setf evil-emacs-state-modes (delq 'pdf-view-mode evil-emacs-state-modes))
(add-to-list 'evil-emacs-state-modes 'durand-greek-search-mode)
(add-to-list 'evil-emacs-state-modes 'account-report-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
(setf evil-search-module 'evil-search)

(setf evil-overriding-maps (delete '(grep-mode-map) evil-overriding-maps))

;; (evil-set-initial-state 'dired-mode 'motion)

(define-key dired-mode-map [?\d] nil)
(define-key dired-mode-map [32] nil)
(define-key dired-mode-map [?x] nil)
(ignore-errors
  (define-key grep-mode-map [?x] nil)
  (define-key grep-mode-map [32] nil))

;; (setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:foreground "cornflowerblue" :background "gray20")))
;;       evil-emacs-state-tag    (propertize " EMACS " 'face '((:foreground "#39bf4c" :background "gray20")))
;;       evil-insert-state-tag   (propertize " INSERT " 'face '((:foreground "green" :background "gray20")))
;;       evil-replace-state-tag  (propertize " REPLACE " 'face '((:foreground "red" :background "gray20")))
;;       evil-motion-state-tag   (propertize " MOTION " 'face '((:foreground "orange" :background "gray20")))
;;       evil-visual-state-tag   (propertize " VISUAL " 'face '((:foreground "goldenrod1" :background "gray20")))
;;       evil-operator-state-tag (propertize " OPERATOR " 'face '((:foreground "pink" :background "gray20"))))

(setq evil-normal-state-tag   (propertize " N " 'face '((:foreground "gray20" :background "cornflowerblue")))
      evil-emacs-state-tag    (propertize " E " 'face '((:foreground "gray20" :background "#39bf4c")))
      evil-insert-state-tag   (propertize " I " 'face '((:foreground "gray20" :background "green")))
      evil-replace-state-tag  (propertize " R " 'face '((:foreground "gray20" :background "red")))
      evil-motion-state-tag   (propertize " M " 'face '((:foreground "gray20" :background "orange")))
      evil-visual-state-tag   (propertize " V " 'face '((:foreground "gray20" :background "goldenrod1")))
      evil-operator-state-tag (propertize " O " 'face '((:foreground "gray20" :background "pink"))))

(evil-define-key nil evil-insert-state-map
  [home] 'evil-normal-state)

(evil-define-key nil evil-visual-state-map
  [home] 'evil-exit-visual-state)

(evil-define-key nil evil-replace-state-map
  [home] 'evil-normal-state)

(evil-define-key nil evil-emacs-state-map
  [home] 'evil-normal-state)


;;;###autoload
(define-prefix-command 'evil-projectile-map)

(define-key evil-projectile-map [?f] 'projectile-find-file)
(define-key evil-projectile-map [?p] 'projectile-switch-project)


;; ret
(define-prefix-command 'durand-evil-ret-map)

(evil-define-key nil durand-evil-ret-map
  "\"" 'transpose-chars-back-4
  ;; [?j] 'jump-to-other-window-link
  ;; [?P] 'evil-backward-sentence-begin
  ;; [?p] 'evil-forward-sentence-begin
  [?s] 'durand-open-browser
  [?,] 'evil-window-top
  [?\r] 'durand-open-link
  [?\;] 'evil-window-middle
  [?:] 'evil-window-bottom
  [?o] 'evil-jump-backward
  [?a] #'durand-agenda
  [?d] #'durand-open-discord
  [?e] 'durand-eval
  ;; [?t]
  ;; (lambda ()
  ;;        (interactive)
  ;;        (if current-prefix-arg
  ;;            (make-process
  ;;             :name "terminal"
  ;;             :command `("open" "-a" "terminal" ,(file-relative-name default-directory))
  ;;             :buffer nil)
  ;;          (eshell)))
  [?t] #'durand-open-terminal
  [?x] 'exchange-point-and-mark
  [?w] 'durand-next-window-or-frame
  [?W] 'ace-select-window
  [?u] 'undo-tree-visualize)

;; spc ret
(define-prefix-command 'durand-evil-spc-ret-map)

(evil-define-key nil durand-evil-spc-ret-map
  [?j] 'bookmark-bmenu-list
  [?g] 'revert-buffer
  [?b] 'org-open-bookmarks
  [?B] 'describe-bindings
  [?f] 'counsel-describe-function
  [?k] 'describe-key
  [?v] 'counsel-describe-variable
  [?l] 'org-open-articles
  [?n] 'org-open-novels
  [?y] 'org-open-youtube
  [?d] 'delete-rectangle
  [?c] 'durand-capture
  [?s] 'durand-start-counting
  [?r] 'string-rectangle)

;;;###autoload
(defun durand-start-counting (&optional arg)
  "With ARG, ask for the number of minutes."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (let ((temps (- (timer-until durand-stop-timer nil))))
      (message (format "Temps restants: %d minutes %d secondes" (/ temps 60) (mod temps 30)))))
   ((null durand-stop-timer)
    (setf durand-stop-timer
          (run-with-timer
           (* (cond
               ((null arg) 60)
               (t
                (read-number "Quels minutes?" 60)))
              60)
           nil
           'durand-stop-reminder))
    (list-timers))
   ((timerp durand-stop-timer)
    (cancel-timer durand-stop-timer)
    (setf durand-stop-timer nil))
   (t
    (user-error "Unknown situation"))))

;; wifi handling

;;;###autoload
(defvar durand-wifi-on-p nil
  "If WIFI is on or not.
This is defined in \"evil-setting.el\"")

;;;###autoload
(defun durand-wifi-filter (proc output)
  "Filter function to set the wifi variable.
This should only be used for the process \"durand-wifi\".
This is defined in \"evil-setting.el\""
  (unless (string= (process-name proc) "durand-wifi")
    (user-error "Filter function applied to a wrong process."))
  (setf durand-wifi-on-p (string-match "On$" output)))

;;;###autoload
(defun durand-wifi ()
  "Check if WIFI is enabled, then ask for confirmation to toggle WIFI.
This is defined in \"custom.el\""
  (interactive)
  (make-process
   :name "durand-wifi"
   :buffer nil
   :command '("networksetup" "-getairportpower" "en0")
   :filter #'durand-wifi-filter
   :sentinel #'ignore)
  (let* ((prompt (format "WIFI is %s. Do you want to turn WIFI %s"
                         (if durand-wifi-on-p "on" "off")
                         (if durand-wifi-on-p "off?" "on?")))
         (decision (y-or-n-p prompt)))
    (when decision
      (let* ((new-state (if durand-wifi-on-p "off" "on")))
        (make-process
         :name "durand-toggle-wifi"
         :buffer nil
         :command `("networksetup"
                    "-setairportpower"
                    "en0"
                    ,new-state)
         :sentinel #'ignore
         :filter #'ignore)
        (message "WIFI turned %s" new-state)))))

;; v map
(defvar durand-view-map (make-keymap)
  "The map for space v. Generally the map for viewing things, like
articles, bookmarks, youtube links, novels, or weblinks.")

(define-key durand-view-map [?l] #'org-open-articles)
(define-key durand-view-map [?b] #'org-open-bookmarks)
(define-key durand-view-map [?y] #'org-open-youtube)
(define-key durand-view-map [?w] #'org-open-weblink)
(define-key durand-view-map [?W] #'durand-wifi)
(define-key durand-view-map [?n] #'org-open-novels)
(define-key durand-view-map [?t] #'durand-view-timers)
(define-key durand-view-map [?P] #'durand-view-process)
(define-key durand-view-map [?p] #'durand-chercher-pdf)
(define-key durand-view-map [?v] #'durand-view-videos)

;; space
(define-prefix-command 'durand-evil-space-map)

(evil-define-key nil durand-evil-space-map
  [tab] 'yas-expand
  ;; [?x] ctl-x-map
  [?n] 'evil-ex-nohighlight
  [?=] 'evil-align-regexp
  ;; [?m] 'durand-mu4e
  ;; [?M] 'mu4e
  [?%] 'widen
  ;; [?u] (lambda ()
  ;;        (interactive)
  ;;        (org-store-link nil t))
  [?v] durand-view-map
  [?é] 'split-window-below
  [?\"] 'split-window-right
  ;; [?X] 'ace-swap-window
  [?o] 'durand-new-buffer
  [?O] 'make-blank-space
  [?c] 'clean-up-buffers
  [?C] 'clean-up-buffers-regex
  [?j] 'durand-bookmark-jump-headlong
  [?i] 'durand-open-index
  [?J] 'bookmark-set
  [?r] 'durand-recentf-jump-headlong
  [?g] 'backward-or-up-sexp
  ;; [?h] 'forward-or-up-sexp
  [?h] help-map
  ;; [?s] 'durand-cap-word
  [?-] 'negative-argument
  [?p] evil-projectile-map
  [?\r] durand-evil-spc-ret-map
  [?b] 'durand-switch-buffer
  [?k] 'kill-current-buffer
  [?d] #'durand-mu4e
  [?f] 'counsel-find-file
  ;; [?'] 'durand-edit-special
  ;; try the newer function
  [?'] 'durand-narrow-dwim
  ;; [?w] 'delete-other-windows
  ;; [?W] 'delete-window
  ;; [?q] 'quit-other-window
  [?z] 'capitalize-region-or-word
  ;; [?t] 'avy-goto-char-timer
  ;; [?:] 'evil-commenter
  ;; [?:] 'durand-comment-dwim
  ;; [?,] 'evil-goto-first-line
  ;; [?\;] 'evil-goto-line
  ;; [?\s-x] (lambda ()
  ;;           (interactive)
  ;;           (setq display-line-numbers
  ;;                 (and (null display-line-numbers)
  ;;                      'relative)))
  ;; [?e] 'durand-wrap-region-with
  )

;;;###autoload
(defun durand-open-index (&optional arg)
  "Open the file \"index.html\" in the default browser.
With ARG, open the index file even not in `js2-mode'."
  (interactive "P")
  (unless (or arg (derived-mode-p 'js2-mode))
    (user-error "One can only open the index file in `js2-mode'"))
  (make-process
   :name "open"
   :buffer nil
   :command (list "open" (if (null arg) "./index.html"
                           (read-string "File name: " "./index.html")))))

;;;###autoload
(defun durand-view-videos ()
  "View videos"
  (interactive)
  (eshell t)
  (insert "vid")
  (eshell-send-input)
  (insert "ls -lah")
  (eshell-send-input)
  (insert "play "))

;; view timer list and process list
;;;###autoload
(defun durand-view-timers ()
  "View the list of timers"
  (interactive)
  (list-timers))

;;;###autoload
(defun durand-view-process (&optional arg)
  "View the list of processes"
  (interactive "P")
  (if arg
      (list-processes)
    (message "%s" (process-list))))

;; $
(define-prefix-command 'durand-evil-dollar-map)

(evil-define-key nil durand-evil-dollar-map
  [?\d] 'back-to-indentation
  [?t] (lambda () (interactive) (recenter 0))
  [?z] (lambda () (interactive) (recenter (/ (window-body-height) 2)))
  [?b] (lambda () (interactive) (recenter -1))
  [?£] 'org-retreat)

;; motion map
(evil-define-key nil evil-motion-state-map
  [home] 'evil-normal-state
  [32] durand-evil-space-map
  [?\r] durand-evil-ret-map
  [?\d] 'durand-other-buffer
  ;; [?$] durand-evil-dollar-map
  [?$] 'evil-end-of-line
  [?i] 'evil-emacs-state
  [40] 'universal-argument
  ;; [?x] 'amx
  [?\)] 'evil-forward-paragraph
  [?Q] 'durand-quit-window)

;; normal mode

 (evil-define-key nil evil-normal-state-map
  [home] 'evil-emacs-state
  ;; [?x] 'amx
  [?X] (lambda ()
         (interactive)
         (pcase major-mode
           ('mu4e-headers-mode
            (mu4e-mark-execute-all t))
           (_
            (user-error "Not in mu4e-headers-mode"))))
  [backspace] #'durand-other-buffer
  [?\d] #'durand-other-buffer
  [32] durand-evil-space-map
  [?\r] durand-evil-ret-map
  ;; [?\)] 'durand-end-of-line-or-block
  [?\)] 'evil-forward-paragraph
  [?s] 'durand-general-save-buffer
  [f10] 'durand-general-save-buffer
  [f9] (lambda () (interactive) (when (functionp durand-tex-action) (funcall durand-tex-action)))
  [?U] 'undo-tree-redo
  [f12] #'undo-tree-undo
  [?t] 'transpose-chars
  [?T] 'durand-show-current-time
  "&" 'transpose-chars-back-2
  "é" 'transpose-chars-back-3
  [?\"] 'evil-use-register
  [?'] 'fill-paragraph
  [?£] 'org-advance
  ;; [?$] durand-evil-dollar-map
  [?$] 'evil-end-of-line
  [?i] 'evil-insert-state
  [?j] 'evil-next-visual-line
  [?k] 'evil-previous-visual-line
  [?+] 'eval-expression
  "," 'evil-repeat-find-char-reverse
  ";" 'evil-repeat-find-char
  ;; [?,] (lambda () (interactive) (durand-buffer-scroll 'up))
  ;; [?\;] (lambda () (interactive) (durand-buffer-scroll 'down))
  ;; [??] (lambda () (interactive) (durand-buffer-scroll 'up 1))
  ;; [32 ?.] (lambda () (interactive) (durand-buffer-scroll 'down 1)) ; I like the repeat operator after all.
  [?.] 'evil-repeat
  [?§] (lambda () (interactive) (evil-scroll-down 0))
  [?è] (lambda () (interactive) (evil-scroll-up 0))
  [?!] (lambda () (interactive) (evil-scroll-line-down 1))
  [?ç] (lambda () (interactive) (evil-scroll-line-up 1))
  [?Q] 'durand-quit-window
  [?p] 'evil-paste-after
  [?\s-m] 'durand-toggle-mode-line
  [?ù] 'evil-goto-mark
  ;; [?S] 'cycle-spacing
  ;; [?z] 'downcase-region-or-word
  ;; [?Z] 'upcase-region-or-word
  [?z] durand-evil-dollar-map
  (kbd "C-SPC") 'set-mark-command
  ;; [?=] 'swiper-isearch
  [?=] 'evil-ex-search-forward
  [?/] 'counsel-grep-or-swiper
  ;; [?<] 'er/expand-region
  ;; [?>] 'er/contract-region
  (kbd "<S-backspace>") 'durand-next-real-buffer
  (kbd "SPC <S-backspace>") 'durand-previous-real-buffer
  [?-] (lambda () (interactive) (durand-buffer-scroll 'up))
  [?_] (lambda () (interactive) (durand-buffer-scroll 'down))
  [?\C--] (lambda () (interactive) (durand-buffer-scroll 'up 1))
  [?\C-_] (lambda () (interactive) (durand-buffer-scroll 'down 1))
  [?\t] (lambda ()
          "org-cycle or indent-for-tab-command"
          (interactive)
          (cond
           ((derived-mode-p 'org-mode)
            (org-cycle current-prefix-arg))
           (t
            (indent-for-tab-command current-prefix-arg))))
  [?J] 'evil-join
  [?\M-o] 'evil-jump-forward
  ;; [?\C-a] 'evil-numbers/inc-at-pt
  ;; [?\C-x] 'evil-numbers/dec-at-pt
  [?\(] 'universal-argument
  [?\s-x] (lambda ()
            (interactive)
            (setq display-line-numbers
                  (and (null display-line-numbers)
                       'relative)))
  [?\à] 'durand-beginning-of-line-or-block)

;; outline mode interferes with my key bindings
(map! :map outline-mode-map
      :n "z" durand-evil-dollar-map
      :map evil-org-mode-map
      :m "gh" 'evil-goto-line)

;; universal argument mode
(define-key universal-argument-map [?\(] 'universal-argument-more)

(defun universal-argument--description ()
  (when prefix-arg
    (concat "("
            (pcase prefix-arg
              (`(-) " -")
              (`(,(and (pred integerp) n))
               (let ((str ""))
                 (while (and (> n 4) (= (mod n 4) 0))
                   (setq str (concat str " ("))
                   (setq n (/ n 4)))
                 (if (= n 4) str (format " %s" prefix-arg))))
              (_ (format " %s" prefix-arg))))))


;; end of visual line

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current visual line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (evil-end-of-visual-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-visual-line
                   vcount)))
  (evil-insert-state 1))

;; align operator

;;;###autoload
(evil-define-operator evil-align-regexp (beg end type &optional arg)
  "Align regions by `align-regexp'.
Treat block selections as selecting lines.
And ARG behaves like in `align-regexp'."
  :move-point nil
  (interactive "<R>P")
  (let ((beg (save-excursion
               (goto-char beg)
               (line-beginning-position)))
        (end (save-excursion
               (goto-char end)
               (line-end-position)))
        (arguments (if arg
                       (list (read-string "Complex align using regexp: "
                                          "\\(\\s-*\\)" 'align-regexp-history)
                             (string-to-number
                              (read-string
                               "Parenthesis group to modify (justify if negative): " "1"))
                             (string-to-number
                              (read-string "Amount of spacing (or column if negative): "
                                           (number-to-string align-default-spacing)))
                             (y-or-n-p "Repeat throughout line? "))
                     (list (concat "\\(\\s-*\\)"
                                   (read-string "Align regexp: "))
                           1 align-default-spacing nil))))
    (save-excursion
      (align-regexp beg end
                    (nth 0 arguments)
                    (nth 1 arguments)
                    (nth 2 arguments)
                    (nth 3 arguments)))))

;; text objects

;; From https://stackoverflow.com/questions/18102004/

(defmacro define-and-bind-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(defmacro define-and-bind-quote-text-object (name key quote-char)
  (let ((inner-name (make-symbol (concat "evil-inner-quote-" name)))
        (outer-name (make-symbol (concat "evil-outer-quote-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-quote ,quote-char beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-quote ,quote-char beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(evil-define-text-object evil-pasted (count &rest args)
  (list (save-excursion (evil-goto-mark ?\[) (point))
        (save-excursion (evil-goto-mark ?\]) (point))))
(define-key evil-inner-text-objects-map "P" 'evil-pasted)

(define-and-bind-quote-text-object "dollar" "$" ?$)
(define-and-bind-text-object "slash" "/" "/" "/")
(define-and-bind-text-object "pipe" "|" "|" "|")
(define-and-bind-text-object "star" "*" "*" "*")
(define-and-bind-text-object "frenchquote" "«" "«" "»")
(define-and-bind-text-object "tikz" "z" "\\(\\\\bpi\\|\\\\tikzpicture\\).*$" "\\(\\\\epi\\|\\\\endtikzpicture\\)")
(define-and-bind-text-object "environment" "e" "\\\\begin{[^{}]+}$" "\\\\end{[^{}]+}")

;; for indentation
;; Now doom-emacs comes equipped with default bindings for indentation text
;; objects.
;; (defun evil-indent--current-indentation ()
;;   "Return the indentation of the current line. Moves point."
;;   (buffer-substring-no-properties (point-at-bol)
;;                                   (progn (back-to-indentation)
;;                                          (point))))

;; (defun evil-indent--same-indent-range (&optional point)
;;   "Return the point at the begin and end of the text block with the same indentation.
;; If `point' is supplied and non-nil it will return the begin and
;; end of the block surrounding point."
;;   (save-excursion
;;     (when point
;;       (goto-char point))
;;     (let ((start (point))
;;           (indent (evil-indent--current-indentation))
;;           begin end)
;;       (cl-loop while (and (/= (point) (point-min))
;;                        (string= (evil-indent--current-indentation) indent))
;;             do (progn
;;                  (setq begin (point-at-bol))
;;                  (forward-line -1)))
;;       (goto-char start)
;;       (cl-loop while (and (/= (point) (point-max))
;;                        (string= (evil-indent--current-indentation) indent))
;;             do (progn
;;                  (setq end (point-at-eol))
;;                  (forward-line 1)))
;;       (list begin end))))

;; (evil-define-text-object evil-indent-a-indent (&optional count beg end type)
;;   "Text object describing the block with the same indentation as
;; the current line and the line above."
;;   :type line
;;   (let ((range (evil-indent--same-indent-range)))
;;     (evil-range (save-excursion
;;                   (goto-char (cl-first (evil-indent--same-indent-range)))
;;                   (forward-line -1)
;;                   (point-at-bol))
;;                 (cl-second range) 'line)))

;; (evil-define-text-object evil-indent-a-indent-lines (&optional count beg end type)
;;   "Text object describing the block with the same indentation as
;; the current line and the lines above and below."
;;   :type line
;;   (let ((range (evil-indent--same-indent-range)))
;;     (evil-range (save-excursion
;;                   (goto-char (cl-first range))
;;                   (forward-line -1)
;;                   (point-at-bol))
;;                 (save-excursion
;;                   (goto-char (cl-second range))
;;                   (forward-line 1)
;;                   (point-at-eol))
;;                 'line)))

;; (evil-define-text-object evil-indent-i-indent (&optional count beg end type)
;;   "Text object describing the block with the same indentation as
;; the current line."
;;   :type line
;;   (let ((range (evil-indent--same-indent-range)))
;;     (evil-range (cl-first range) (cl-second range) 'line)))

;;;###autoload
;; (eval-after-load 'evil
;;   '(progn
;;      (define-key evil-inner-text-objects-map "i" 'evil-indent-i-indent)
;;      (define-key evil-outer-text-objects-map "i" 'evil-indent-a-indent)
;;      (define-key evil-outer-text-objects-map "I" 'evil-indent-a-indent-lines)))
