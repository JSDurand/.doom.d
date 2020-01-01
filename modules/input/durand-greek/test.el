(let ((s "*bbh*aa"))
  (durand-greek-search-replace-generic "\\*\\(.\\)"
                                       'upcase
                                       :str-p s
                                       :sub-exp 1)
  s)


;; Τὸν άσφαλέστερον τρόπον
;; *to\n a/sfale/steron tro/pon
