;; init-elfeed.el --- Initialize elfeed.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; A RSS feed reader.
;;

;;; Code:

(use-package elfeed
  :pretty-hydra
  ((:title (pretty-hydra-title "Elfeed" 'faicon "rss-square")
           :color amaranth :quit-key "q")
   ("Search"
    (("g" elfeed-search-update--force "refresh")
     ("G" elfeed-search-fetch "update")
     ("y" elfeed-search-yank "copy URL")
     ("+" elfeed-search-tag-all "tag all")
     ("-" elfeed-search-untag-all "untag all"))
    "Filter"
    (("s" elfeed-search-live-filter "live filter")
     ("S" elfeed-search-set-filter "set filter")
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
     ("A" (elfeed-search-set-filter "@6-months-ago" "all"))
     ("T" (elfeed-search-set-filter "@1-day-ago" "today")))
    "Article"
    (("b" elfeed-search-browse-url "browse")
     ("n" next-line "next")
     ("p" previous-line "previous")
     ("u" elfeed-search-tag-all-unread "mark unread")
     ("r" elfeed-search-untag-all-unread "mark read")
     ("RET" elfeed-search-show-entry "show"))))
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("?" . elfeed-hydra/body)
         :map elfeed-show-mode-map
         ("o" . ace-link)
         ("q" . delete-window))
  :config
  (setq elfeed-db-directory (locate-user-emacs-file ".elfeed")
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'delete-window
        elfeed-feeds '("http://planet.emacsen.org/atom.xml"
                       "http://www.masteringemacs.org/feed/"
                       "https://oremacs.com/atom.xml"
                       "https://pinecast.com/feed/emacscast"
                       "https://www.reddit.com/r/emacs.rss")))

(provide 'init-elfeed)

;;; init-elfeed.el ends here
