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


;;add system environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
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
              recentf-exclude '((expand-file-name package-user-dir)
                                ".cache"
                                ".cask"
                                ".elfeed"
                                "bookmarks"
                                "cache"
                                "ido.*"
                                "persp-confs"
                                "recentf"
                                "undo-tree-hist"
                                "url"
                                "COMMIT_EDITMSG\\'")))

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
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(use-package simple
  :ensure nil
  :hook ((window-setup . size-indication-mode)
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
;; SmoothScroll
;; Vertical Scroll

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)


;; Misc
(setq-default fill-column 100)
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t
      inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

(provide 'init-base)

;;init-base.el ends here
