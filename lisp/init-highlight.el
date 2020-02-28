;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Highlighting configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-}" . symbol-overlay-jump-next)
         ("M-{" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1
              symbol-overlay-faces
              '((:inherit (highlight bold))
                (:inherit (font-lock-builtin-face bold) :inverse-video t)
                (:inherit (warning bold) :inverse-video t)
                (:inherit (font-lock-constant-face bold) :inverse-video t)
                (:inherit (error bold) :inverse-video t)
                (:inherit (dired-mark bold) :inverse-video t)
                (:inherit (success bold) :inverse-video t)
                (:inherit (font-lock-keyword-face bold) :inverse-video t)))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

;; Highlight indentions
(when *sys/gui*  (use-package highlight-indent-guides
                   :diminish
                   :functions (ivy-cleanup-string
                               my-ivy-cleanup-indentation)
                   :commands highlight-indent-guides--highlighter-default
                   :functions  my-indent-guides-for-all-but-first-column
                   :hook (prog-mode . highlight-indent-guides-mode)
                   :init (setq highlight-indent-guides-method 'character
                               highlight-indent-guides-character ?\| ;; candidates: , ⋮, ┆, ┊, ┋, ┇
                               highlight-indent-guides-responsive 'top
                               highlight-indent-guides-auto-enabled nil
                               highlight-indent-guides-auto-character-face-perc 10
                               highlight-indent-guides-auto-top-character-face-perc 20)
                   :config
                   ;; Don't display indentations while editing with `company'
                   (with-eval-after-load 'company
                     (add-hook 'company-completion-started-hook
                               (lambda (&rest _)
                                 "Trun off indentation highlighting."
                                 (when highlight-indent-guides-mode
                                   (highlight-indent-guides-mode -1))))
                     (add-hook 'company-after-completion-hook
                               (lambda (&rest _)
                                 "Trun on indentation highlighting."
                                 (when (and (derived-mode-p 'prog-mode)
                                            (not highlight-indent-guides-mode))
                                   (highlight-indent-guides-mode 1)))))

                   ;; Don't display first level of indentation
                   (defun my-indent-guides-for-all-but-first-column (level responsive display)
                     (unless (< level 1)
                       (highlight-indent-guides--highlighter-default level responsive display)))
                   (setq highlight-indent-guides-highlighter-function #'my-indent-guides-for-all-but-first-column)

                   ;; Disable `highlight-indent-guides-mode' in `swiper'
                   ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
                   (with-eval-after-load 'ivy
                     (defun my-ivy-cleanup-indentation (str)
                       "Clean up indentation highlighting in ivy minibuffer."
                       (let ((pos 0)
                             (next 0)
                             (limit (length str))
                             (prop 'highlight-indent-guides-prop))
                         (while (and pos next)
                           (setq next (text-property-not-all pos limit prop nil str))
                           (when next
                             (setq pos (text-property-any next limit prop nil str))
                             (ignore-errors
                               (remove-text-properties next pos '(display nil face nil) str))))))
                     (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :functions (my-rainbow-colorize-match my-rainbow-clear-overlays)
  :commands(rainbow-x-color-luminance rainbow-colorize-match rainbow-turn-off)
  :bind (:map help-mode-map
              ("w" . rainbow-mode))
  :hook ((css-mode scss-mode less-css-mode) . rainbow-mode)
  :config
  ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  (defun my-rainbow-colorize-match (color &optional match)
    (let* ((match (or match 0))
           (ov (make-overlay (match-beginning match) (match-end match))))
      (overlay-put ov 'ovrainbow t)
      (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                "white" "black"))
                              (:background ,color)))))
  (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

  (defun my-rainbow-clear-overlays ()
    (remove-overlays (point-min) (point-max) 'ovrainbow t))
  (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :bind (:map hl-todo-mode-map
         ([C-f3] . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))
  (setq hl-todo-keyword-faces
        '(("NOTE" . "#d0bf8f")
          ("HOLD" . "#dca3a3"))))

;; Highlight uncommitted changes
(use-package diff-hl
  :defines (diff-hl-margin-symbols-alist desktop-minor-mode-table)
  :commands diff-hl-magit-post-refresh
  :functions my-diff-hl-fringe-bmp-function
  :custom-face (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector #b11100000)
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (unless *sys/gui*    (setq diff-hl-margin-symbols-alist
                             '((insert . " ") (delete . " ") (change . " ")
                               (unknown . " ") (ignored . " ")))
          ;; Fall back to the display margin since the fringe is unavailable in tty
          (diff-hl-margin-mode 1)
          ;; Avoid restoring `diff-hl-margin-mode'
          (with-eval-after-load 'desktop
            (add-to-list 'desktop-minor-mode-table
                         '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Highlight some operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
;; Pulse current line
(use-package pulse
  :ensure nil
  :preface
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point) 'next-error))

  (defun my-pulse-momentary (&rest _)
    "Pulse the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (dolist (cmd '(recenter-top-bottom
                 other-window ace-window windmove-do-window-select
                 pager-page-down pager-page-up
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))
  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse)))

(provide 'init-highlight)


;;; init-highlight.el ends here
