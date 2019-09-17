;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary

;;; Code

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold (if (display-graphic-p) 400000000 100000000))

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;;; early-init.el ends here
