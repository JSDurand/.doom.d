;;; completion/durand-completion/autoload.el -*- lexical-binding: t; -*-

;; NOTE: The main functionality that I desire out of a completion framework.
;;;###autoload
(defun durand-choose-list (cands &optional all texte non-quick display-cadr no-require-match
                                 no-sort)
  "Choose from an alist CANDS. Multiple selection is supported.
If NO-SORT is non-nil, then don't sort the CANDS.
TEXTE is the prompt to use if non-nil.
If ALL is non-nil, add a choice to select all of them.
If NON-QUICK is nil, then offer the selection even when there is only one candidate.
If DISPLAY-CADR is non-nil, then display cadr rather than car.
If NO-REQUIRE-MATCH is t, then don't require the selection to match."
  (if (and (= (length cands) 1) (null non-quick))
      (list (caar cands))
    (let ((cands (if all (cons '("all") cands) cands))
          (question (or texte "Chois un: "))
          selection-cands)
      (setf durand-choose-list-result nil
            cands
            (mapcar (lambda (x)
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
      ;; if no-sort, then don't sort!
      (setf selection-cands
            (cond
             (no-sort
              (lambda (string pred action)
                (cond
                 ((eq action 'metadata)
                  '(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity)))
                 (t
                  (complete-with-action
                   action cands string pred)))))
             (t
              (mapcar #'car cands))))
      (setf durand-choose-list-result
            (icomplete-vertical-do '(:height (/ (frame-height) 4))
              (completing-read-multiple question selection-cands
                                        nil (not no-require-match))))
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

(defvar durand-headlong-entered-minibuffer-p nil
  "Whether or not we have entered minibuffer.
This is used for determining if we shall exit the minibuffer when
there is only one candidate left.")

;;;###autoload
(defun durand-headlong-minibuffer-setup-hook ()
  "The entry for the completion to be headlong.
Simply add this function to `minibuffer-setup-hook'."
  ;; NOTE: When we run this function we first enter minibuffer, so we set a
  ;; variable to its appropriate value.
  (setf durand-headlong-entered-minibuffer-p nil)
  (add-hook 'post-command-hook 'durand-headlong-post-command-hook t)
  (add-hook 'minibuffer-exit-hook 'durand-headlong-minibuffer-exit-hook))

;;;###autoload
(defun durand-headlong-post-command-hook ()
  "Exit the minibuffer if there is only one candidate left.
In practice, when we first enter minibuffer, there is some
abnormal behaviour, so we only check when we have entered the
minibuffer as usual."
  (let ((comps (completion-all-sorted-completions)))
    (when comps
      (setf (cdr (last comps)) nil) )
    (cond
     ((and durand-headlong-entered-minibuffer-p
           (= (length comps) 1))
      (minibuffer-force-complete-and-exit))
     (t (setf durand-headlong-entered-minibuffer-p t)))))

;;;###autoload
(defun durand-headlong-minibuffer-exit-hook ()
  "Remove the hooks we added."
  (remove-hook 'post-command-hook 'durand-headlong-post-command-hook)
  (remove-hook 'minibuffer-setup-hook 'durand-headlong-minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook 'durand-headlong-minibuffer-exit-hook))

;;;###autoload
(defun contrib/completing-read-in-region (start end collection &optional predicate)
  "Prompt for completion of region in the minibuffer if non-unique.
Use as a value for `completion-in-region-function'."
  (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
      (completion--in-region start end collection predicate)
    (let* ((initial (buffer-substring-no-properties start end))
           (limit (car (completion-boundaries initial collection predicate "")))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all)))
                         (concat (substring initial 0 limit) (car all)))
                        (t (completing-read
                            "Completion: " collection predicate t initial)))))
      (if (null completion)
          (progn (message "No completion") nil)
        (delete-region start end)
        (insert completion)
        t))))

;;;###autoload
(defun prot/icomplete-minibuffer-truncate ()
    "Truncate minibuffer lines in `icomplete-mode'.
  This should only affect the horizontal layout and is meant to
  enforce `icomplete-prospects-height' being set to 1.

  Hook it to `icomplete-minibuffer-setup-hook'."
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))

;;; show no candidates without input for certain functions

;;;###autoload
(defun prot/icomplete-empty-input-no-list (&rest _args)
  "Advice to initially hide candidates in `icomplete-mode'."
  (interactive
   (lambda (old-interactive-spec)
     (let ((icomplete-show-matches-on-no-input nil))
       (ignore icomplete-show-matches-on-no-input)
       (advice-eval-interactive-spec old-interactive-spec)))))

(defun durand-icomplete-empty-input-no-list (old-fun &rest args)
  "Advice to initially hide candidates in `icomplete-mode'.
Add an around advice to the function to effect."
  )

;;;###autoload
(defun prot/icomplete-yank-kill-ring ()
  "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.

Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
  (interactive)
  (let ((kills                          ; do not sort items
         (lambda (string pred action)
           (if (eq action 'metadata)
               '(metadata (display-sort-function . identity)
                          (cycle-sort-function . identity))
             (complete-with-action
              action kill-ring string pred)))))
    (icomplete-vertical-do
        '(:separator 'dashed-line :height (/ (frame-height) 4))
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert
       (completing-read "Yank from kill ring: " kills nil t)))))

;;;###autoload
(defun prot/embark-live-occur-single-buffer ()
  "Do not spawn multiple `embark-live-occur' buffers.
Add this to `minibuffer-exit-hook'."
  (let ((eo "*Embark Live Occur*"))
    (when (get-buffer eo)
      (kill-buffer eo))))

;;;###autoload
(defun contrib/with-embark-live-occur (&rest _args)
  "Advice to wrap function in `embark-completing-read'."
  (interactive
   (lambda (old-interactive-spec)
     (let ((icomplete-mode nil)
           (embark-occur-view 'grid)
           (completing-read-function #'embark-completing-read))
       (ignore icomplete-mode embark-occur-view
               completing-read-function)
       (advice-eval-interactive-spec old-interactive-spec)))))

;;;###autoload
(defun durand-icomplete-vertical (&rest _args)
  "Advice to wrap function to use vertical display in icomplete."
  (interactive
   (lambda (old-interactive-spec)
     (icomplete-vertical-do '(:separator "\n"
                              :height (/ (frame-height) 4))
       (advice-eval-interactive-spec
        old-interactive-spec)))))
