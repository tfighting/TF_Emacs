;;; init-search.el --- -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This initializes  color-rg snails
;;

;; Code

(eval-when-compile
  (require 'init-const))

;; A fuzzy match tool
;; git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
;; ~/.fzf/install
(use-package fzf)

;; Code restructuring and search tools.
(use-package color-rg
  :ensure nil
  :if "rg"
  :bind ("C-M-s" . color-rg-search-input))

;; a awesome fuzzy match search framework.
(use-package snails
  :ensure nil
  :if *sys/gui*
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :config
  (use-package exec-path-from-shell
    :if *sys/mac-cocoa*)
  ;; Install fuz.el, the download page:https://apt.llvm.org/
  ;; sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
  ;; sudo apt install clang
  ;; some fuz configuration
  (add-to-list 'load-path (expand-file-name "site-lisp/fuz" user-emacs-directory))
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
