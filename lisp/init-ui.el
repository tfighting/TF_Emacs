;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant)
  (require 'init-custom))


;;;;;;;;;;;;;;;;;;;;Some basic interface configuration ;;;;;;;;;;;;;;;;;;;;

;; Forbid some GUI features
(setq use-file-dialog nil
      use-dialog-box nil)


;; Logo

(setq fancy-splash-image t_fighting-logo)

;; Title
(setq frame-title-format '("T_Finghting Emacs - %b")
      icon-title-format frame-title-format)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Some Mac interface configuration.
(when *sys/mac-gui*  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      (add-hook 'after-load-theme-hook
                (lambda ()
                  (let ((bg (frame-parameter nil 'background-mode)))
                    (set-frame-parameter nil 'ns-appearance bg)
                    (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

(when *sys/mac*  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Icons Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(when (and *sys/gui* t_fighting-display-icons)
  (use-package all-the-icons
    :init (unless (or *sys/win32p* (font-installed-p "all-the-icons"))
	        (all-the-icons-install-fonts t))
    :config
    (with-no-warnings
      ;; FIXME: Align the directory icons
      ;; @see https://github.com/domtronn/all-the-icons.el/pull/173
      (defun all-the-icons-icon-for-dir (dir &optional chevron padding)
        "Format an icon for DIR with CHEVRON similar to tree based directories."
        (let* ((matcher (all-the-icons-match-to-alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist))
               (path (expand-file-name dir))
               (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
               (padding (or padding "\t"))
               (icon (cond
                      ((file-symlink-p path)
                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.0))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.0))
                      ((file-exists-p (format "%s/.git" path))
                       (format "%s" (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.0)))
                      (t (apply (car matcher) (list (cadr matcher) :v-adjust 0.0))))))
          (format "%s%s%s%s%s" padding chevron padding icon padding))))

    (defun all-the-icons-reset ()
      "Reset (unmemoize/memoize) the icons."
      (interactive)
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (ignore-errors
          (memoize-restore f)
          (memoize f)))
      (message "Reset all-the-icons"))

    (add-to-list 'all-the-icons-icon-alist
                 '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
    (add-to-list 'all-the-icons-icon-alist
                 '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
    (add-to-list 'all-the-icons-icon-alist
                 '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
    (add-to-list 'all-the-icons-icon-alist
                 '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :ace all-the-icons-green))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue))))

;;;;;;;;;;;;;;;;;;;;;;;;; Theme Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;


(cond (t_fighting-load-theme
       (use-package doom-themes
         :init
         (load-theme 'doom-vibrant t)
         :config
         ;; Global settings (defaults)
         (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
               doom-themes-enable-italic t) ; if nil, italics is universally disabled


         ;; Enable flashing mode-line on errors
         (doom-themes-visual-bell-config)

         ;; Enable custom neotree theme (all-the-icons must be installed!)
         (doom-themes-neotree-config)
         ;; or for treemacs users
         (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
         (doom-themes-treemacs-config)

         ;; Corrects (and improves) org-mode's native fontification.
         (doom-themes-org-config)))

      ((and  *sys/gui* (not t_fighting-load-theme))
       (set-background-color "#002B36")
       (set-foreground-color "white")))

(provide 'init-ui)

;;; init-ui.el ends here.
