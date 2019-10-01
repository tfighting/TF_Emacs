;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Use mirror
(setq package-archives '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("org"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)


;; A modern Packages Menu
(use-package paradox
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (setq paradox-display-star-count nil)

  (defalias 'upgrade-packages #'paradox-upgrade-packages)

  ;; Replace default `list-packages'
  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                (let ((buf (get-buffer-create "*Paradox Report*"))
                      (inhibit-read-only t))
                  (with-current-buffer buf
                    (page-break-lines-mode 1))))
              t)))

(provide 'init-package)

;;init-package.el ends here
