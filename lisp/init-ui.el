;;; init-ui.el --- Ui configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: UI Configuration

;; Logo
(setq fancy-splash-image t_fighting-logo)

;; Titile
(setq frame-title-format '("T_Finghting Emacs - %b")
      icon-title-format frame-title-format)


;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)


;; Theme
(use-package doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Hide minor mode
(use-package minions
  :hook (after-init . minions-mode))


(provide 'init-ui)

;;;; init-ui.el ends here.
