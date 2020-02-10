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
  (with-no-warnings
    (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                        &key face height v-adjust)
      "Add an icon in the hydra title."
      (let ((face (or face `(:foreground ,(face-background 'highlight))))
            (height (or height 1.0))
            (v-adjust (or v-adjust 0.0)))
        (concat
         (when (and (display-graphic-p) icon-type icon-name)
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
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number" :toggle (if (fboundp 'display-line-numbers-mode)
                                   display-line-numbers-mode
                                 global-linum-mode))
        ("a" aggressive-indent-mode "aggressive indent" :toggle t)
        ("h" hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("S" prettify-symbols-mode "pretty symbol" :toggle t)
        ("L" page-break-lines-mode "page break lines" :toggle t)
        ("M" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("l" global-hl-line-mode "line" :toggle t)
        ("P" show-paren-mode "paren" :toggle t)
        ("s" symbol-overlay-mode "symbol" :toggle t)
        ("r" rainbow-mode "rainbow" :toggle t)
        ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("i" highlight-indent-guides-mode "indent" :toggle t)
        ("T" hl-todo-mode "todo" :toggle t))
       "Coding"
       (("f" flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("o" origami-mode "folding" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
       "Version Control"
       (("v" diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("m" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Theme"
       (("t d" (t_fighting-load-theme 'default) "default"
         :toggle (eq t_fighting-theme 'default))
        ("t c" (t_fighting-load-theme 'classic) "classic"
         :toggle (eq t_fighting-theme 'classic))
        ("t r" (t_fighting-load-theme 'colorful) "colorful"
         :toggle (eq t_fighting-theme 'colorful))
        ("t k" (t_fighting-load-theme 'dark) "dark"
         :toggle (eq t_fighting-theme 'dark))
        ("t l" (t_fighting-load-theme 'light) "light"
         :toggle (eq t_fighting-theme 'light))
        ("t y" (t_fighting-load-theme 'day) "day"
         :toggle (eq t_fighting-theme 'day))
        ("t n" (t_fighting-load-theme 'night) "night"
         :toggle (eq t_fighting-theme 'night))
        ("t o" (ivy-read "Load custom theme: "
                         (mapcar #'symbol-name
                                 (custom-available-themes))
                         :predicate (lambda (candidate)
                                      (string-prefix-p "doom-" candidate))
                         :action (lambda (theme)
                                   (setq t_fighting-theme
                                         (let ((x (intern theme)))
                                           (or (car (rassoc x t_fighting-theme-alist))
                                               x)))
                                   (counsel-load-theme-action theme))
                         :caller 'counsel-load-theme)
         "others" :toggle (not (assoc t_fighting-theme t_fighting-theme-alist))))

       ))))

(provide 'init-hydra)


;;; init-hydra.el ends here
