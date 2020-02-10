;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some usefule Utilities.
;;

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map youdao-dictionary-mode-map
         ("h" . youdao-dictionary-hydra/body)
         ("?" . youdao-dictionary-hydra/body))
  :init
  (use-package posframe)
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (with-no-warnings
    (defun my-youdao-search-at-point ()
      "Search word at point and display result with `posframe', `pos-tip', or buffer."
      (interactive)
      (if (display-graphic-p)
          (if emacs/>=26p
              (youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-tooltip))
        (youdao-dictionary-search-at-point))))
  :config
  (with-eval-after-load 'hydra
    (defhydra youdao-dictionary-hydra (:color blue)
      ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
      ("y" youdao-dictionary-play-voice-at-point "play voice at point")
      ("q" quit-window "quit")
      ("C-g" nil nil)
      ("h" nil nil)
      ("?" nil nil))))

;;
;; Search tools
;;

;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode)))


;; PDF reader
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
    :commands pdf-view-midnight-minor-mode
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init (setq pdf-annot-activate-created-annotations t)
    :config
    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when sys/macp
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Set dark theme
    (defun my-pdf-view-set-midnight-colors ()
      "Set pdf-view midnight colors."
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'default))))

    (defun my-pdf-view-set-dark-theme ()
      "Set pdf-view midnight theme as color theme."
      (my-pdf-view-set-midnight-colors)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq major-mode 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))

    (my-pdf-view-set-midnight-colors)
    (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)


    ;; FIXME: Support retina
    ;; @see https://emacs-china.org/t/pdf-tools-mac-retina-display/10243/
    ;; and https://github.com/politza/pdf-tools/pull/501/
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)
    (with-no-warnings
      (defun pdf-view-use-scaling-p ()
        "Return t if scaling should be used."
        (and (or (and (eq system-type 'darwin) (string-equal emacs-version "27.0.50"))
                 (memq (pdf-view-image-type)
                       '(imagemagick image-io)))
             pdf-view-use-scaling))
      (defun pdf-view-create-page (page &optional window)
        "Create an image of PAGE for display on WINDOW."
        (let* ((size (pdf-view-desired-image-size page window))
               (width (if (not (pdf-view-use-scaling-p))
                          (car size)
                        (* 2 (car size))))
               (data (pdf-cache-renderpage
                      page width width))
               (hotspots (pdf-view-apply-hotspot-functions
                          window page size)))
          (pdf-view-create-image data
                                 :width width
                                 :scale (if (pdf-view-use-scaling-p) 0.5 1)
                                 :map hotspots
                                 :pointer 'arrow))))

    ;; Recover last viewed position
    (when emacs/>=26p
      (use-package pdf-view-restore
        :hook (pdf-view-mode . pdf-view-restore-mode)
        :init (setq pdf-view-restore-filename
                    (locate-user-emacs-file ".pdf-view-restore"))))))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :hook (olivetti-mode . (lambda ()
                           (if olivetti-mode
                               (text-scale-set +2)
                             (text-scale-set 0))))
  :init (setq olivetti-body-width 0.618))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :init (setq atomic-chrome-buffer-open-style 'frame)
  :config
  (if (fboundp 'gfm-mode)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode)))))

;; Music player
(use-package bongo
  :functions (bongo-add-dired-files
              dired-get-filename
              dired-marker-regexp
              dired-move-to-filename)
  :commands (bongo-buffer
             bongo-library-buffer-p
             bongo-library-buffer)
  :bind ("C-<f9>" . bongo)
  :init
  (with-eval-after-load 'dired
    (defun bongo-add-dired-files ()
      "Add marked files to Bongo library"
      (interactive)
      (bongo-buffer)
      (let (file (files nil))
        (dired-map-over-marks
         (setq file (dired-get-filename)
               files (append files (list file)))
         nil t)
        (with-bongo-library-buffer
         (mapc 'bongo-insert-file files)))
      (bongo-switch-buffers))
    (bind-key "b" #'bongo-add-dired-files dired-mode-map)))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment)          ; List all enviroment varialbe.
(use-package memory-usage)
(use-package ztree)                     ; show files like tree

(provide 'init-utils)


;;; init-utils.el ends here
