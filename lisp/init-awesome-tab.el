 ;;; init-awesome-tab.el --- Better toggle tab Configuration.	-*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;
;; Better toggle tab Configuration.
;;


;;; Code:

;;Some configuration can be modified in ~/.emacs.d/site-lisp/awesome-tab/awesome-tab.el
(use-package awesome-tab
  :ensure nil
  :init
  (require 'awesome-tab)
  (when *sys/gui*
    (require 'all-the-icons))

  :config
  (awesome-tab-mode t)
  (setq awesome-tab-label-max-length 20)

  (unless *sys/gui*
    (setq awesome-tab-display-icon nil
          frame-background-mode 'dark))
  (when *sys/gui*
    (setq awesome-tab-show-tab-index t)
    ;; Use s-n switch tab.
    (global-set-key (kbd "M-1") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-2") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-3") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-4") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-5") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-6") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-7") 'awesome-tab-select-visible-tab)
    (global-set-key (kbd "M-8") 'awesome-tab-select-visible-tab))

  :bind
  ("<f7>" . awesome-tab-ace-jump)
  ("C-<f7>" . awesome-tab-switch-group))


(provide 'init-awesome-tab)
;;; init-awesome-tab.el ends here.
