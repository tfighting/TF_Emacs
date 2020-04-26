;;; init-aweshell.el --- awesome shell -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: awesome shell

(use-package aweshell
  :ensure nil
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("<f9>" . aweshell-dedicated-open)
   (:map eshell-mode-map ("<f9>" . aweshell-dedicated-close))))


(use-package shell-here
  :bind ("M-~" . shell-here)
  :config
  (when *sys/linux*
    (setq explicit-shell-file-name "/bin/bash")))


(use-package multi-term
  :ensure nil
  :commands (multi-term)
  :bind
  (("M-<f9>" . multi-term)
   (:map dired-mode-map ("M-<f9>" . multi-term)))
  :custom
  (multi-term-program (executable-find "bash"))
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("<M-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete))))

(provide 'init-aweshell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-aweshell.el ends here
