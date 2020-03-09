;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;; Display buffer icons on GUI
(when t_fighting-display-icons
  (use-package all-the-icons-ibuffer
    :if *sys/gui*
    :init (all-the-icons-ibuffer-mode 1)))

;; Ibuffers groups via project.
(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (and *sys/gui* t_fighting-display-icons)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust -0.05
                                    :height 1.25)
             " ")
          "Project: ")))

(provide 'init-ibuffer)

;;; init-buffer.el ends here
