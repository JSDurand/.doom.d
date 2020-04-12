;;; editor/durand-evil/autoload.el -*- lexical-binding: t; -*-

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
  "Check if WIFI is enabled, then ask for confirmation to toggle WIFI. "
  (interactive)
  (make-process
   :name "durand-wifi"
   :buffer nil
   :command '("networksetup" "-getairportpower" "en0")
   :filter #'durand-wifi-filter
   :sentinel #'ignore)
  (accept-process-output (get-process "durand-wifi"))
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

;;;###autoload
;; (defun universal-argument--description ()
;;   (when prefix-arg
;;     (concat "C-u"
;;             (pcase prefix-arg
;;               (`(-) " -")
;;               (`(,(and (pred integerp) n))
;;                (let ((str ""))
;;                  (while (and (> n 4) (= (mod n 4) 0))
;;                    (setq str (concat str " C-u"))
;;                    (setq n (/ n 4)))
;;                  (if (= n 4) str (format " %s" prefix-arg))))
;;               (_ (format " %s" prefix-arg))))))

;;;###autoload
(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current visual line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one.
Modified by Durand"
  (interactive "p")
  (after! evil
    (evil-end-of-visual-line)
    (setq evil-insert-count count
          evil-insert-lines nil
          evil-insert-vcount
          (and vcount
               (> vcount 1)
               (list (line-number-at-pos)
                     #'end-of-visual-line
                     vcount)))
    (evil-insert-state 1)))

;; align operator

;;;###autoload (autoload 'evil-align-regexp "editor/durand-evil/autoload" nil t)
(evil-define-operator evil-align-regexp (beg end _type &optional arg)
  "Align regions by `align-regexp'.
Treat block selections as selecting lines.
And ARG behaves like in `align-regexp'.
Modified by Durand"
  :move-point nil
  (interactive "<R>P")
  (let* ((default-spacing (if (boundp 'align-default-spacing)
                              align-default-spacing
                            1))
         (beg (save-excursion
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
                                            (number-to-string default-spacing)))
                              (y-or-n-p "Repeat throughout line? "))
                      (list (concat "\\(\\s-*\\)"
                                    (read-string "Align regexp: "))
                            1 default-spacing nil))))
    (save-excursion
      (align-regexp beg end
                    (nth 0 arguments)
                    (nth 1 arguments)
                    (nth 2 arguments)
                    (nth 3 arguments)))))

;; text objects

;; From https://stackoverflow.com/questions/18102004/

;;;###autoload (autoload 'define-and-bind-text-object "editor/durand-evil/autoload" nil t 'macro)
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

;;;###autoload (autoload 'define-and-bind-quote-text-object "editor/durand-evil/autoload" nil t 'macro)
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
