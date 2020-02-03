;;; init-aweshell.el --- -*- lexical-binding: t -*-

;;
;; Commentary
;;

;; Code


(eval-when-compile
  (require 'init-const))

;; enhance eshell
(use-package aweshell
  :load-path (lambda () (expand-file-name "site-lisp/aweshell" user-emacs-directory))
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("M-#" . aweshell-dedicated-open)
   (:map eshell-mode-map ("M-#" . aweshell-dedicated-close))))

;; `cd' to frequent directory in `eshell',similarity to jump-mode of zsh.
(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))


;; enhance shell
(use-package multi-term
  :load-path (lambda () (expand-file-name "site-lisp/multi-term" user-emacs-directory))
  :commands (multi-term)
  :bind
  (([f9] . multi-term)
   (:map dired-mode-map ([f9] . multi-term)))
  :custom
  (multi-term-program (executable-find "zsh")))


(use-package term-keys
  :if (not sys/gui)
  :config (term-keys-mode t))



(provide 'init-aweshell)
