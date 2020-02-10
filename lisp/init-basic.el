;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Better defaults.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Personal information
(setq user-full-name t_fighting-full-name)
(setq user-mail-address t_fighting-mail-address)


;; Compatibility
(unless (fboundp 'caadr)
  (defun caadr (x)
    "Return the `car' of the `car' of the `cdr' of X."
    (declare (compiler-macro internal--compiler-macro-cXXr))
    (car (car (cdr x)))))



;; Encoding
;; UTF-8 as the default coding system
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

;;add system environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH" "MANPATH")
          exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 200
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
                "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format nil
              display-time-day-and-date nil))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init (setq column-number-mode t
              line-number-mode t
              kill-whole-line t               ; Kill line including '\n'
              line-move-visual nil
              track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
              set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))


;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)

(when sys/gui
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;;
;; Misc
;;

;; Don't lock files
(setq-default create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default marjor-mode 'text-mode
	     fill-column 80
	     tab-width 4
	     indent-tabs-mode nil) ;; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Fullscreen
;; WORKAROUND: To address blank screen issue with child-frame in fullscreen
(when (and sys/mac-x-p emacs/>=26p)
  (add-hook 'window-setup-hook (lambda ()
                                 (setq ns-use-native-fullscreen nil))))
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
           ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("S-s-<return>" . toggle-frame-fullscreen)
           ("M-S-<return>" . toggle-frame-fullscreen))


(provide 'init-basic)

;;init-base.el ends here
