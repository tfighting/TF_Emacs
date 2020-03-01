;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Directory configurations.
;;

;;; Code:


(eval-when-compile
  (require 'init-constant))

;; Directory operations
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
        dired-recursive-copies 'always
        dired-dwim-target t)

  (when *sys/mac*    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and *sys/mac* (executable-find "gls"))
            (and (not *sys/mac*) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  ;; buffer sorts via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
           ("C-z s" . hydra-dired-quick-sort/body)))

  ;; Allow copy files from dired-mode to the select files.
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync))))
;; Shows icons
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
