;;; init-mark.el --- A efficient mark configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>

;; Keywords:Easy mark.

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Jump to things in Emacs tree-style.
(use-package avy
  :bind ("C-z g" . avy-hydra/body)
  :hook (after-init . avy-setup-default)
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'pre)

  (pretty-hydra-define avy-hydra (:title (pretty-hydra-title "Goto Anywhere" 'wicon "train")
                                  :color amaranth :quit-key "q")
    ("Char"
     (("c" avy-goto-char "goto char" :exit t)
      ("t" avy-goto-char-2 "goto 2 char" :exit t))

     "word"
     (("w" avy-goto-word-1 "goto word" :exit t)
      ("e" avy-goto-word-0 "goto everywhere" :exit t))

     "Line"
     (("l" avy-goto-line "goto line" :exit t))))

  ;; Mark to everywhere.
  (defun t_fighting-mark-to-char ()
    (interactive)
    (call-interactively #'set-mark-command)
    (call-interactively #'avy-goto-char))
  (global-set-key (kbd "C-z m") 't_fighting-mark-to-char)
  (global-set-key (kbd "<C-return>") 't_fighting-mark-to-char))





(provide 'init-mark)
;;; init-mark.el ends here.
