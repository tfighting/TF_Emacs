 ;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;
;; T_Fighting Emacs - A Fancy and Fast Emacs Configuration.
;; Only support emacs27 above.


;;; Code:

;; emacs--version >= 27
(when (version< emacs-version "27")
  (warn "T-fighting Emacs requires 27 and above"))

;;Speed up startup
(defvar t_fighting-gc-cons-threshold  80000000
  "The default value to use for `gc-cons-threshold'.
If you experience freezing,decrease this. If you experience stuttering, increase this.")

(defvar t_fighting-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

;; Better GC threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold t_fighting-gc-cons-threshold)))

;; GC automatically while unfocusing the frame
;; `focus-out-hook' is obsolete since 27.1
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* t_fighting-gc-cons-threshold 2)))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold t_fighting-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
;;if you set the third libraty,you can cancel the commentary
(add-subdirs-to-load-path)

;;Constants
(require 'init-constant)
(require 'init-custom)
(require 'init-package)
(require 'init-modeline)
(require 'init-basic-config)
(require 'init-functions)
(require 'init-fonts)
(require 'init-hydra)
(require 'init-ivy)
(require 'init-ui)
(require 'init-highlight)
(require 'init-edit)
(require 'init-mark)
(require 'init-kill)
(require 'init-jump)
(require 'init-search)
(require 'init-eaf)
(require 'init-awesome-tab)
(require 'init-aweshell)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-window)
(require 'init-treemacs)
(require 'init-markdown)
(require 'init-pdf)
(require 'init-utils)
(require 'init-org)
(require 'init-pyim)

;;Programming
(require 'init-vcs)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-lsp)

;;script language
(require 'init-elisp)
(require 'init-python)
(require 'init-prog)

;; Happy Games
(require 'init-games)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(browse-url-browser-function 'eaf-open-browser)
 '(counsel-find-file-at-point t)
 '(counsel-yank-pop-separator "
────────
")
 '(custom-safe-themes
   '("4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "0f0a4dca8bb029dc5139f447ff25bc3c18d31872c30a46d03c6bbc706ded3586" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" default))
 '(eaf-find-alternate-file-in-dired t t)
 '(enable-recursive-minibuffers t)
 '(iny-height 10 t)
 '(ivy-count-format "【%d/%d】")
 '(ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
 '(ivy-on-del-error-function nil)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffer nil t)
 '(ivy-wrap t)
 '(lsp-auto-guess-root t)
 '(lsp-file-watch-threshold 2000)
 '(lsp-keep-workspace-alive nil)
 '(lsp-prefer-flymake nil t)
 '(make-backup-files nil)
 '(multi-term-program "/usr/bin/zsh" t)
 '(org-agenda-window-setup 'other-window)
 '(org-catch-invisible-edits 'smart)
 '(org-confirm-babel-evaluate nil)
 '(org-ellipsis "  ☕")
 '(org-hide-emphasis-markers t)
 '(org-log-done 'time)
 '(org-pretty-entities nil)
 '(org-priority-faces '((65 . error) (66 . warning) (67 . success)))
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-startup-indented t)
 '(org-tags-column -80)
 '(org-todo-keyword-faces '(("HANGUP" . warning)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROCESS(p)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")))
 '(org-use-speed-commands t)
 '(read-process-output-max 1048576 t)
 '(sp-escape-quotes-after-insert nil)
 '(super-save-auto-save-when-idle t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold t)))))
 '(diff-hl-change ((t (:foreground "#7c6f64"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(hl-line ((t (:extend t))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(lsp-ui-doc-background ((t (:background "lightyellow"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:background "#3c3836"))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(snails-content-buffer-face ((t (:background "#111" :height 110))))
 '(snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
 '(snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
 '(symbol-overlay-default-face ((t (:inherit (region bold))))))
