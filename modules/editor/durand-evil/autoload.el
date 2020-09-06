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
This is defined in \"/Users/durand/.doom.d/modules/editor/durand-evil/autoload.el\"")

;;;###autoload
(defvar durand-bluetooth-on-p nil
  "If BLUETOOTH is on or not.
This is defined in \"/Users/durand/.doom.d/modules/editor/durand-evil/autoload.el\"")

;;;###autoload
(defun durand-wifi-filter (proc output)
  "Filter function to set the wifi variable.
This should only be used for the process \"durand-wifi\".
This is defined in \"evil-setting.el\""
  (unless (string= (process-name proc) "durand-wifi")
    (user-error "Filter function applied to a wrong process."))
  (setf durand-wifi-on-p (string-match "On$" output)))

;;;###autoload
(defun durand-bluetooth-filter (proc output)
  "Filter function to set the bluetooth variable.
This should only be used for the process \"durand-bluetooth\".
This is defined in \"/Users/durand/.doom.d/modules/editor/durand-evil/autoload.el\""
  (unless (string= (process-name proc) "durand-bluetooth")
    (user-error "Filter function applied to a wrong process."))
  (setf durand-bluetooth-on-p (string-match "1" output)))

;;;###autoload
(defun durand-wifi-or-bluetooth (&optional arg)
  "Check if WIFI is enabled, then ask for confirmation to toggle WIFI.
If ARG is non-nil, do the same for bluetooth."
  (interactive "P")
  (cond
   ((null arg)
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
   (t
    (make-process
     :name "durand-bluetooth"
     :buffer nil
     :command '("blueutil" "-p")
     :filter #'durand-bluetooth-filter
     :sentinel 'ignore)
    (accept-process-output (get-process "durand-bluetooth"))
    (let* ((prompt (format "BLUETOOTH is %s. Do you want to turn BLUETOOTH %s"
                           (if durand-bluetooth-on-p "on" "off")
                           (if durand-bluetooth-on-p "off?" "on?")))
           (decision (y-or-n-p prompt)))
      (when decision
        (let* ((new-state (if durand-bluetooth-on-p "0" "1")))
          (make-process
           :name "durand-toggle-bluetooth"
           :buffer nil
           :command `("blueutil" "-p" ,new-state)
           :sentinel 'ignore
           'filter 'ignore)
          (message "BLUETOOTH turned %s"
                   (if durand-bluetooth-on-p "off" "on"))))))))

(define-obsolete-function-alias 'durand-wifi 'durand-wifi-or-bluetooth "2020-09-01"
  "Check if WIFI is enabled, then ask for confirmation to toggle WIFI.")

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
  ;; (insert "cd ~/Desktop/Centre/Vidéos")
  (insert "vid")
  (eshell-send-input)
  ;; (insert "alias play \"mpv --no-terminal --autofit=100%x100% --no-border --geometry=+0+-24 \\$*\"")
  (eshell-send-input)
  (insert "ls -lah")
  (eshell-send-input)
  (insert "play "))

;; view timer list and process list
;;;###autoload
(defun durand-view-timers-or-temps (&optional arg)
  "View the list of timers or view the CPU temperature info.
If ARG is nil, view the list of timers.
If ARG is '(4), view the information about CPU temperature and
fans speed and some others.
If ARG is '(16), view the battery information."
  (interactive "P")
  (cond
   ((null arg) (list-timers))
   ((equal arg (list 4))
    (let (fan-speed cpu-die-temperature)
      (with-temp-buffer
        "*CPU and fans*" nil nil
        (insert (funcall
                 (plist-get (car (auth-source-search :host "local-computer"))
                            :secret)))
        (call-process-region
         nil nil "sudo"
         nil t nil "-S" "powermetrics"
         "-i1" "-n1" "-ssmc")
        (goto-char (point-min))
        (search-forward "Fan" nil t)
        (setf fan-speed
              (progn (re-search-forward "[[:digit:]]+" (line-end-position) t)
                     (string-to-number (match-string 0)))
              cpu-die-temperature
              (progn (re-search-forward "temperature: \\([[:digit:]]+\\.[[:digit:]]+\\)" nil t)
                     (string-to-number (match-string 1)))))
      (message "fan: %d, temp: %s"
               fan-speed cpu-die-temperature)))
   ((equal arg (list 16))
    (let (remain full-capacity fullp charging cycle condition connected)
      (with-temp-buffer
        "*Battery info*" nil nil
        (call-process "system_profiler" nil t nil
                      "SPPowerDataType")
        (goto-char (point-min))
        (re-search-forward "Charge Remaining (mAh): \\([[:digit:]]+\\)" nil t)
        (setf remain (string-to-number (match-string-no-properties 1)))
        (re-search-forward "Fully Charged: \\(.+\\)$" nil t)
        (setf fullp (match-string-no-properties 1))
        (re-search-forward "Charging: \\(.+\\)$" nil t)
        (setf charging (match-string-no-properties 1))
        (re-search-forward "Full Charge Capacity (mAh): \\([[:digit:]]+\\)$" nil t)
        (setf full-capacity (string-to-number (match-string-no-properties 1)))
        (re-search-forward "Cycle Count: \\(.+\\)$" nil t)
        (setf cycle (string-to-number (match-string-no-properties 1)))
        (re-search-forward "Condition: \\(.+\\)$" nil t)
        (setf condition (match-string-no-properties 1))
        (re-search-forward "Connected: \\(.+\\)$" nil t)
        (setf connected (match-string-no-properties 1)))
      (message "Full: %d, remaining: %d, fullp: %s, charging: %s, connected: %s, cycles: %d, condition: %s"
               full-capacity remain fullp charging connected cycle condition)))
   (t
    (user-error "Unsupported ARG: %S" arg))))

(define-obsolete-function-alias 'durand-view-timers 'durand-view-timers-or-temps
  "2020-09-02" "View the list of timers.")

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
(evil-define-operator evil-align-regexp (beg end type &optional arg)
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
