;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package auto-save
  :ensure nil
  :init
  (require 'auto-save)
  (auto-save-enable)

  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t))  ; automatically delete spaces at the end of the line when saving

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; parenthesis operation
(use-package awesome-pair
  :ensure nil
  :bind
  (:map prog-mode-map
   (("C-l" . awesome-pair-jump-out-pair-and-newline)
    ("SPC" . awesome-pair-space)
    ("M-f" . awesome-pair-jump-right)
    ("M-b" . awesome-pair-jump-left)))
  :hook (prog-mode . awesome-pair-mode))

;; Delete Block
(use-package delete-block
  :ensure nil
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))


;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map))
  (when (featurep 'xwidget-internal)
    (bind-key "C-c C-z w" #'xwidget-webkit-browse-url)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package ace-link
  :defines (org-mode-map
            gnus-summary-mode-map
            gnus-article-mode-map
            ert-results-mode-map)
  :bind ("M-o" . ace-link-addr)
  :hook (after-init . ace-link-setup-default)
  :config
  (with-eval-after-load 'org
    (bind-key "M-o" #'ace-link-org org-mode-map))
  (with-eval-after-load 'gnus
    (bind-keys
     :map gnus-summary-mode-map
     ("M-o" . ace-link-gnus)
     :map gnus-article-mode-map
     ("M-o" . ace-link-gnus)))
  (with-eval-after-load 'ert
    (bind-key "o" #'ace-link-help ert-results-mode-map)))


;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                         'java-mode 'go-mode 'swift-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Redefine M-< and M-> for some modes

(use-package beginend
  :diminish (beginend-mode beginend-global-mode)
  :hook (after-init . beginend-global-mode)
  :config
  (mapc (lambda (pair)
          (add-hook (car pair) (lambda () (diminish (cdr pair)))))
        beginend-modes))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ;

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))


;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Undo/Redo
(use-package undo-fu
  :bind (([remap undo] . undo-fu-only-undo)
         ([remap undo-only] . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)
         ("M-_" . undo-fu-only-redo)))

;; Goto last change
(use-package goto-last-change
  :bind ("s-," . goto-last-change))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Hideshow
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind ("C-`" . hs-toggle-hiding))



;; Open files as another user
(unless *sys/win32p*  (use-package sudo-edit))




(provide 'init-edit)

;;init-edit.el ends here
