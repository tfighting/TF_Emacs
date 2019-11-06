;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Jump to definition

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist
                '(("lambda" . ?λ)
                  ("<-" . ?←)
                  ("->" . ?→)
                  ("->>" . ?↠)
                  ("=>" . ?⇒)
                  ("map" . ?↦)
                  ("/=" . ?≠)
                  ("!=" . ?≠)
                  ("==" . ?≡)
                  ("<=" . ?≤)
                  (">=" . ?≥)
                  ("=<<" . (?= (Br . Bl) ?≪))
                  (">>=" . (?≫ (Br . Bl) ?=))
                  ("<=<" . ?↢)
                  (">=>" . ?↣)
                  ("&&" . ?∧)
                  ("||" . ?∨)
                  ("not" . ?¬)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Jump to definition
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "anchor")
    :color blue :quit-key "q")
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (:map dumb-jump-mode-map
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy))


(use-package editorconfig
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c x" . quickrun)))

;; misc
(use-package csv-mode)  ;; adjust csv file

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

;; Fish shell
;; (use-package fish-mode
;;   :hook (fish-mode . (lambda ()
;;                        (add-hook 'before-save-hook
;;                                  #'fish_indent-before-save))))

(provide 'init-prog)


;;; init-prog.el ends here
