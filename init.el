;; This configuration only for orgmode and latex-mode. -*- lexical-binding: t; -*-
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Startup Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Speed up startup
(defvar t_fighting-gc-cons-threshold  16000000
  "The default value to use for `gc-cons-threshold'.
If you experience freezing,decrease this. If you experience stuttering, increase this.")

(defvar t_fighting-gc-cons-upper-limit  400000000
  "The temporary value for `gc-cons-threshold' to defer it.")



(defvar t_fighting-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(setq gc-cons-threshold t_fighting-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
(lambda ()
	"Restore defalut values after startup."
	(setq file-name-handler-alist default-file-name-handler-alist)
	(setq gc-cons-threshold t_fighting-gc-cons-threshold
				gc-cons-percentage 0.1)

	;; GC automatically while unfocusing the frame
	;; `focus-out-hook' is obsolete since 27.1
	(if (boundp 'after-focus-change-function)
			(add-function :after after-focus-change-function
										(lambda ()
											(unless (frame-focus-state)
												(garbage-collect))))
		(add-hook 'focus-out-hook 'garbage-collect))

	;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
	;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
	(defun my-minibuffer-setup-hook ()
		(setq gc-cons-threshold t_fighting-gc-cons-upper-limit))

	(defun my-minibuffer-exit-hook ()
		(setq gc-cons-threshold t_fighting-gc-cons-threshold))

	(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
	(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Use mirror ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(setq package-archives '(
			 ("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org"   . "http://elpa.emacs-china.org/org/")
			 ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))

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

(require 'init-custom)
(require 'init-package)
(require 'init-basic)
(require 'init-fonts)
(require 'init-hydra)
(require 'init-ui)
(require 'init-functions)
(require 'init-edit)
(require 'init-dired)
(require 'init-dashboard)
(require 'init-company)
(require 'init-highlight)
(require 'init-eaf)
(require 'init-awesome-tab)
(require 'init-aweshell)
(require 'init-org)
(require 'init-markdown)
(require 'init-projectile)
(require 'init-windows)
(require 'init-vcs)
(require 'init-utils)
(require 'init-python)
