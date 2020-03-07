;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Better defaults.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant)
  (require 'init-custom))

;; Personal information
(setq user-full-name t_fighting-full-name)
(setq user-mail-address t_fighting-mail-address)

;;;;;;;;;;;;;;;;;;;; System Coding UTF-8;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;add system environment
(when (or *sys/mac-gui* *sys/linux-gui*)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH"))
    (exec-path-from-shell-initialize)))

(use-package super-save
  :diminish
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil)
  (make-backup-files nil)
  :hook (after-init . super-save-mode))

;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))
(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

;;
;; Misc
;;

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)

;; Inhibit startup configuration.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display line number when files are opend
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Highlight the current line
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t)))) ; FIXME: compatible with 27
  :hook (after-init . global-hl-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; global set keybindings;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cancel keybindings
(global-set-key (kbd "C-z") nil)

;; Delete whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default marjor-mode 'text-mode
	          fill-column 80
	          tab-width 4
	          indent-tabs-mode nil) ;; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)


;; When buffer is closed, saves the cursor location.
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))


;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)

(when *sys/gui*  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
                       mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Fullscreen
;; WORKAROUND: To address blank screen issue with child-frame in fullscreen
(when *sys/mac-gui*
  (add-hook 'window-setup-hook (lambda ()
                                 (setq ns-use-native-fullscreen nil))))
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
           ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("S-s-<return>" . toggle-frame-fullscreen)
           ("M-S-<return>" . toggle-frame-fullscreen))


(provide 'init-basic-config)

;;init-base.el ends here
