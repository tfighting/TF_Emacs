;;; init-search.el --- -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This initializes ivy swiper counsel color-rg snails
;;

;; Code


;; Search some files/variable etc. in the current project.
(use-package color-rg
  :load-path (lambda () (expand-file-name "site-lisp/color-rg" user-emacs-directory))
  :if (executable-find "rg")
  :bind ("C-M-s" . color-rg-search-input))


(use-package snails
  :load-path (lambda () (expand-file-name "site-elisp/snails/" user-emacs-directory))
  :if (display-graphic-p)
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :config
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))

  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))

  ;; Some backends for windows or mac.
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything snails-backend-mdfind)))
  :bind
  (("M-s s" . snails)
   ("M-s p" . snails-current-project)
   ("M-s b" . snails-active-recent-buffers)
   ("M-s e" . snails-everywhere)))


(provide 'init-search)
