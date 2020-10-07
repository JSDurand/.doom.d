;;; app/durand-weather/autoload.el -*- lexical-binding: t; -*-

;;; NOTE: essential library

(require 'cl-lib)

;;;###autoload
(cl-defstruct weather-condition
  "A struct which holds the various information about the weather."
  chanceofrain
  tempfeel
  temp
  windspeed
  desc
  humidity)

;;;###autoload
(defun durand-weather-parse-into-struct (input)
  "Parse the INPUT into the strcut WEATHER-CONDITION."
  ;;; TODO
  input)

;;;###autoload
(defun durand-weather ()
  "Show weather information"
  (interactive)
  (with-temp-buffer
    (call-process "curl" nil t nil
                  "-H" "Accept-Language: fr"
                  "wttr.in/Taipei\\?format=j1")
    (goto-char (point-min))
    (search-forward "{" nil t)
    (forward-char -1)
    (save-excursion
      (cl-loop while (search-forward "" nil t)
               do (setf (buffer-substring (1- (point))
                                          (1+ (line-end-position)))
                        "")))
    (let* ((inhibit-read-only t)
           weather-alist weather result-list)
      (setf weather-alist (json-parse-buffer :object-type 'alist :array-type 'list)
            ;; current (alist-get 'current_condition weather-alist nil nil 'equal)
            weather (alist-get 'weather weather-alist nil nil 'equal))
;;; TODO: now parse the data and display them
      (setf result-list (durand-weather-parse-into-struct weather))
;;; TODO: display the result
      result-list)))
