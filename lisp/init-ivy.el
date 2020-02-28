;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:
(eval-when-compile
  (require 'init-custom))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook
  ((after-init . ivy-mode)
   (ivy-mode . counsel-mode))

  :bind (("C-s" . swiper-isearch)
         ("C-." . imenu)
         ("C-x C-r" . counsel-buffer-or-recentf)
         ("C-c B" . counsel-bookmarked-directory)
         ("C-c g" . counsel-ag) ;; sudo apt install silversearcher-ag
         ("C-c r" . counsel-rg) ;; sudo apt install rigrep
         ("C-c f" . counsel-fzf) ;; install fzf
         ("C-c l" . counsel-locate)
         ("C-c o" . counsel-outline)


         :map counsel-mode-map
         ([remap dired] . counsel-dired)
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-u" . counsel-up-directory)
         ("C-h" . counsel-goto-home-directory))


  :custom
  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (ivy-use-virtual-buffer nil)
  (iny-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  (counsel-find-file-at-point t)
  (counsel-yank-pop-separator "\n────────\n")

  :config
  (defun counsel-goto-home-directory ()
    "Go to the $HOME of the local machine."
    (interactive)
    (ivy--cd "~/"))

  ;;;;;;;;;;;;;;;;;;;; Display an arrow with the selected item;;;;;;;;;;;;;;;;;;;;;;
  (defun my-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if *sys/gui*                    (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                 ">")
               (propertize " " 'display `(space :align-to 2))
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n"))
  (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)


  ;;;;;;;;;;;;;;;;;;;;;;; Better sorting and filtering;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package prescient
    :commands prescient-persist-mode
    :init
    (setq prescient-filter-method '(literal regexp initialism fuzzy))
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (setq ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist
          '((counsel-ag . ivy-prescient-non-fuzzy)
            (counsel-rg . ivy-prescient-non-fuzzy)
            (counsel-pt . ivy-prescient-non-fuzzy)
            (counsel-grep . ivy-prescient-non-fuzzy)
            (counsel-imenu . ivy-prescient-non-fuzzy)
            (counsel-yank-pop . ivy-prescient-non-fuzzy)
            (swiper . ivy-prescient-non-fuzzy)
            (swiper-isearch . ivy-prescient-non-fuzzy)
            (swiper-all . ivy-prescient-non-fuzzy)
            (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
            (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
            (insert-char . ivy-prescient-non-fuzzy)
            (counsel-unicode-char . ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder))
          ivy-prescient-sort-commands
          '(:not swiper swiper-isearch ivy-switch-buffer
                 counsel-grep counsel-git-grep counsel-ag counsel-imenu
                 counsel-yank-pop counsel-recentf counsel-buffer-or-recentf))

    (ivy-prescient-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :commands ivy-hydra-read-action
    :init (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :commands ivy-yasnippet--preview
    :bind ("C-c C-y" . ivy-yasnippet)
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; Quick launch apps
  (cond
   (*sys/linux-gui*    (bind-key "s-<f6>" #'counsel-linux-app counsel-mode-map))
   (*sys/mac*    (use-package counsel-osx-app
                   :bind (:map counsel-mode-map
                               ("s-<f6>" . counsel-osx-app)))))


  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :hook ((counsel-projectile-mode . ivy-rich-mode) ; Must load after `counsel-projectile'
           (ivy-rich-mode . (lambda ()
                              "Use abbreviate in `ivy-rich-mode'."
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init (setq ivy-rich-parse-remote-buffer nil) ; For better performance
    :config
    ;; Better experience with icons
    (use-package all-the-icons-ivy-rich
      :if *sys/gui*
      :init (all-the-icons-ivy-rich-mode 1))))

(provide 'init-ivy)

;;; init-ivy.el ends here.
