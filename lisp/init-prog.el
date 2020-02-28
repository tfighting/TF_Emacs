;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Prettify symbols
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("<-" . ?←)
          ("->" . ?→)
          ("->>" . ?↠)
          ("=>" . ?⇒)
          ("/=" . ?≠)
          ("!=" . ?≠)
          ("==" . ?≡)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("=<<" . (?= (Br . Bl) ?≪))
          (">>=" . (?≫ (Br . Bl) ?=))
          ("<=<" . ?↢)
          (">=>" . ?↣))))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

;; Batch Mode eXtras
(use-package bmx-mode
  :after company
  :diminish
  :hook (after-init . bmx-mode-setup-defaults))

;; misc
(use-package csv-mode)  ;; adjust csv file

(provide 'init-prog)

;;; init-prog.el ends here
