;;init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant))
;;pip install pylint
(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
