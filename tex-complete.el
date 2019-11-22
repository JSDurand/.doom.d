;; Write the completion backend for tex. Both company and the original
;; built-in completion-at-point do not give me much luck. I think I
;; shall build my own completion system, on top of ivy: either
;; ivy-read, or ivy-completing-read, or both.
;; After some time I found that the macro names can be completed by default, so there is
;; no need to write my own backends now...

;; (require 'cl-lib)
;; (require 'company)

;;;###autoload
;; (defun rough-fuzzy-match (prefix cand)
;;   "Return t if PREFIX is a subset of CAND"
;;   (string-match-p (mapconcat #'identity
;; 			     (split-string prefix "")
;; 			     ".*")
;; 		  cand))

;;;###autoload
;; (defun find-macro-content (macro)
;;   "Find the content of a tex macro"
;;   (let* ((ind (string-match "{" macro))
;; 	 (content (substring macro 0 ind)))
;;     (cond
;;      ((string-equal content "\\defonetext")
;;       (substring macro (1+ ind) -1))
;;      ((string-equal content "\\deftwotext")
;;       (substring macro (1+ (string-match "{" macro (1+ ind))) -1))
;;      (t
;;       (substring macro (1+ ind) -1)))))

;;;###autoload
;; (defun durand-prettify (str)
;;   "
;; Replace all spaces with a space and truncate to 50"
;;   (truncate-string-to-width (replace-regexp-in-string "\n+" " " str) 50))

;;;###autoload
;; (defun company-tex-backend (command &optional arg &rest ignored)
;;   "A backend for use in plain-tex-mode.

;; It searches for macros and offers to complete them."
;;   (interactive (list 'interactive))
;;   (let* ((defs (get-defs))
;; 	 (def-names
;; 	   (mapcar (lambda (x) (string-trim x "\\\\"))
;; 		   (mapcar (lambda (y) (find-macro-name (car y)))
;; 			   defs)))
;; 	 (defs-alist (cl-mapcar 'cons def-names defs)))
;;     (cl-case command
;;       (interactive (company-begin-backend 'company-tex-backend))
;;       (prefix (and (eq major-mode 'plain-tex-mode)
;; 		   (company-grab-word)))
;;       (candidates (remove-if-not
;; 		   (lambda (c)
;; 		     (rough-fuzzy-match arg c))
;; 		   def-names))
;;       (require-match nil)
;;       (doc-buffer (company-doc-buffer (cadr (assoc arg defs-alist))))
;;       (annotation (format "%d" (cddr (assoc arg defs-alist))))
;;       (meta (durand-prettify
;; 	     (find-macro-content (cadr (assoc arg defs-alist))))))))

;; (add-to-list 'company-backends 'company-tex-backend)

;; (with-eval-after-load 'tex-mode
;;   (define-key plain-tex-mode-map [tab] 'completion-at-point)
;;   (add-to-list 'completion-at-point-functions 'durand-tex-completion-at-point-function))




;;;###autoload
;; (defun durand-tex-completion-at-point-function ()
;;   (require 'subr-x)
;;   (let* ((defs (get-defs))
;; 	 (def-names
;; 	   (mapcar (lambda (x) (string-trim x "\\\\"))
;; 		   (mapcar (lambda (y) (find-macro-name (car y))) defs))))
;;     (let ((bounds (bounds-of-thing-at-point 'word)))
;;       (cond (bounds
;; 	     (list (car bounds)
;; 		   (cdr bounds)
;; 		   def-names
;; 		   :exclusive 'no))
;; 	    (t
;; 	     nil)))))

;; (add-to-list 'completion-at-point-functions 'durand-tex-completion-at-point-function)


