;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nice looking hydras.
;;

;;; Code:


(eval-when-compile
  (require 'init-custom))

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and *sys/gui* icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                      :color amaranth :quit-key "q")
    ("Basic"
     (("n" display-line-numbers-mode "line number" :toggle t)
      ("a" aggressive-indent-mode "aggressive indent" :toggle t)
      ("d" hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("s" prettify-symbols-mode "pretty symbol" :toggle t)
      ("l" page-break-lines-mode "page break lines" :toggle t)
      ("b" display-battery-mode "battery" :toggle t))
     "Highlight"
     (("h l" global-hl-line-mode "line" :toggle t)
      ("h p" show-paren-mode "paren" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
     "Version Control"
     (("v" diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("M" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t)))))

(provide 'init-hydra)


;;; init-hydra.el ends here
