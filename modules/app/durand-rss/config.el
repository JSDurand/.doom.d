;;; app/durand-rss/config.el -*- lexical-binding: t; -*-

;; feeds
;; NOTE: I put my feeds in ~/org/elfeed.org now.
;; (setq elfeed-feeds
;;       '(;; ("http://nullprogram.com/feed/"
;; 	;;  program)
;; 	;; ("http://planet.emacsen.org/atom.xml"
;; 	;;  emacs
;; 	;;  blog)
;; 	("https://lukesmith.xyz/rss.xml"
;; 	 luke
;; 	 blog)
;; 	("http://notrelated.libsyn.com/rss"
;; 	 luke
;; 	 relevant
;; 	 podcast)
;; 	;; ("https://stackexchange.com/feeds/tagsets/347224/favorite-tags?sort=active"
;; 	;;  stackexchange
;; 	;;  favorite)
;; 	;; ("https://stackexchange.com/feeds/tagsets/347226/real-love?sort=active"
;; 	;;  real-love
;; 	;;  interests)
;; 	("https://www.reddit.com/r/emacs/.rss"
;; 	 emacs
;; 	 reddit)
;; 	("https://math.stackexchange.com/feeds/tag?tagnames=number-theory&sort=newest"
;; 	 interests
;; 	 favorite)
;; 	("https://mattbaker.blog/feed/"
;; 	 interests
;; 	 mattbaker
;; 	 blog)
;; 	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTfRwznpxtbjQQQJ_15Fk2w"
;; 	;;  youtube
;; 	;;  3M)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"
;; 	 youtube
;; 	 3blue1brown)
;; 	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyC_4jvPzLiSkJkLIkA7B8g"
;; 	;;  youtube
;; 	;;  music
;; 	;;  lindsey)
;; 	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCPRWWKG0VkBA0Pqa4Jr5j0Q"
;; 	;;  youtube
;; 	;;  joeman)
;; 	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCjhwHd3mgmqm0ONm0bXKmng"
;; 	;;  youtube
;; 	;;  anju)
;; 	("https://www.youtube.com/feeds/videos.xml?channel_id=UCcXhhVwCT6_WqjkEniejRJQ"
;; 	 wintergarten
;; 	 youtube)
;;         ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtR5okwgTMghi_uyWvbloEg"
;;          lao_kao
;;          youtube)
;;         ("https://www.youtube.com/feeds/videos.xml?channel_id=UCI4xp8qHD1MDErkqxb1dPbA"
;;          innerFrench
;;          youtube)
;;         ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTx8xR4VZ46IAsp5yBgLMzw"
;;          endClashRoyale
;;          youtube)
;;         ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8TtAsZE51ekqffnNASo7DA"
;;          dacon
;;          youtube)
;;         ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMUnInmOkrWN4gof9KlhNmQ"
;;          KaoetMuo
;;          youtube)
;;         ("https://www.youtube.com/feeds/videos.xml?channel_id=UCwYJs4-yKmaALkLFX1uHFsw"
;;          drama
;;          youtube)
;; 	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqripRcC8scod22F5NKvLcQ"
;; 	;;  julia
;; 	;;  youtube)
;; 	;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKSiE8dIEWsT-1jQCGrOqtw"
;; 	;;  youtube
;; 	;;  little-white)
;; 	;; ("https://math.stackexchange.com/feeds/question/2883754"
;; 	;;  math
;; 	;;  relation
;; 	;;  important)
;; 	;; ("https://haskellweekly.news/haskell-weekly.atom"
;; 	;;  haskell
;; 	;;  relevant
;; 	;;  blog)
;; 	("https://themonadreader.wordpress.com/feed/"
;; 	 monad-reader
;; 	 blog
;; 	 important)
;; 	;; ("https://www.reddit.com/r/haskell/.rss"
;; 	;;  haskell
;; 	;;  reddit)
;; 	;; ("https://www.archlinux.org/feeds/news/"
;; 	;;  archlinux
;; 	;;  relevant)
;; 	))

(map! :leader :n [?e] '=rss)

(use-package! elfeed
  :commands elfeed
  :config
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
  (add-to-list 'elfeed-search-face-alist
               '(math elfeed-math-face))
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
  ;; (define-key elfeed-search-mode-map [?i] 'important-tag-toggler)
  ;; (define-key elfeed-search-mode-map [?l] 'relevant-tag-toggler)
  ;; (define-key elfeed-search-mode-map [?e] 'emacs-tag-toggler)
  ;; (define-key elfeed-search-mode-map [?m] 'math-toggler)
  ;; (define-key elfeed-search-mode-map [?U] 'unread-tag-toggler)
  ;; (define-key elfeed-search-mode-map [?y] 'youtube-tag-toggler)
  ;; (define-key elfeed-search-mode-map [?h] 'haskell-tag-toggler)
  (define-key elfeed-search-mode-map [?b] 'elfeed-visit-or-play-with-mpv)
  (define-key elfeed-search-mode-map [?d] 'elfeed-download-youtube)
  (define-key elfeed-search-mode-map [?n] #'elfeed-next-entry)
  (define-key elfeed-search-mode-map [?p] #'elfeed-previous-entry)
  (define-key elfeed-show-mode-map [?b] 'elfeed-visit-or-play-with-mpv)

  (map! :map elfeed-search-mode-map
        :n [?b] 'elfeed-visit-or-play-with-mpv
        :n [?s] 'durand-elfeed-filter-complete
        :n [?g ?r] 'elfeed-search-fetch)

  (setq-default elfeed-search-filter "@1week-ago +unread"))

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
