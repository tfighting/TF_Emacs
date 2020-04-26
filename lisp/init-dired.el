;;; init-dired.el --- Dired Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Dired mode

(use-package dired
  :ensure nil
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode) ; change names in dired-mode
         ("o" . (lambda () (interactive) (find-alternate-file "..")))
         ("<RET>" . dired-find-alternate-file))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Using `insert-directory-program'
  (setq ls-lisp-use-insert-directory-program t)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")


  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
                ("S" . hydra-dired-quick-sort/body)))

  ;; Allow copy files from dired-mode to the select files.
  (use-package dired-rsync
    :bind (:map dired-mode-map
		        ("C-c C-r" . dired-rsync))))

(provide 'init-dired)

;;; init-dired.el ends here.
