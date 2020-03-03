;; init-kill.el --- Initialize kill-ring configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Kill ring configurations.
;;

;;; Code:

;; Cut text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package thing-edit
  :demand t
  :ensure nil
  :after pretty-hydra
  :config
  (with-no-warnings
    (pretty-hydra-define thing-edit-hydra (:title (pretty-hydra-title "Easy Edit" 'wicon "train")
                                           :color amaranth :quit-key "q")
      ("Copy"
       (("c w" thing-copy-word "word" :exit t)
        ("c s" thing-copy-sexp "symbol" :exit t)
        ("c f" thing-copy-defun "defun" :exit t)
        ("c u" thing-copy-url "url" :exit t)
        ("c l" thing-copy-region-or-line "line or region" :exit t))

       "Kill"
       (("k w" thing-cut-word "word" :exit t)
        ("k s" thing-cut-sexp "smybol" :exit t)
        ("k f" thing-cut-defun "defun" :exit t)
        ("k u" thing-cut-url "url" :exit t)
        ("k l" thing-cut-defun "line or region" :exit t)))))
  :bind ("C-z e" . thing-edit-hydra/body)
  )


(provide 'init-kill)
;;; init-kill-ring.el ends here.
