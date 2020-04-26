;;; init-company.el --- company any ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook
  ((after-init . ivy-mode)
   (ivy-mode . counsel-mode))

  :bind (("C-s" . swiper-isearch)
         ("C-." . imenu)
         ("C-x C-r" . counsel-buffer-or-recentf)
		 ("C-c r" . counsel-rg)     ;; Suport search content in the current project.
         ("C-c m" . counsel-bookmarked-directory)
		 :map counsel-mode-map
         ([remap dired] . counsel-dired)
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-goto-home-directory))


  :custom
  (ivy-re-builders-alist
   '((read-file-name-internal . ivy--regex-fuzzy)
	 (t . ivy--regex-plus)))

  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (ivy-use-virtual-buffer nil)
  (ivy-use-selectable-prompt t)
  (iny-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "[%d/%d]")
  (ivy-wrap t)
  (counsel-find-file-at-point t)
  (counsel-yank-pop-separator "\n————————\n")


  :init
  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s"))

  :config
  (defun counsel-goto-home-directory ()
    "Go to the org path."
    (interactive)
    (ivy--cd "~/org"))

  (with-no-warnings
    ;; Display an arrow with the selected item
    (defun my-ivy-format-function-arrow (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                          (>= (length str) 1)
                          (string= " " (substring str 0 1)))
                     ">"
                   "> ")
                 (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                          (>= (length str) 1)
                          (string= " " (substring str 0 1)))
                     " "
                   "  ")
                 str))
       cands
       "\n"))
    (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))
    (defvar-local my-ivy-fly--travel nil)

    (defun my-ivy-fly-back-to-present ()
      (cond ((and (memq last-command my-ivy-fly-commands)
                  (equal (this-command-keys-vector) (kbd "M-p")))
             ;; repeat one time to get straight to the first history item
             (setq unread-command-events
                   (append unread-command-events
                           (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command '(self-insert-command
                                      ivy-forward-char
                                      ivy-delete-char delete-forward-char
                                      end-of-line mwim-end-of-line
                                      mwim-end-of-code-or-line mwim-end-of-line-or-code
                                      yank ivy-yank-word counsel-yank-pop))
                 (equal (this-command-keys-vector) (kbd "M-n")))
             (unless my-ivy-fly--travel
               (delete-region (point) (point-max))
               (when (memq this-command '(ivy-forward-char
                                          ivy-delete-char delete-forward-char
                                          end-of-line mwim-end-of-line
                                          mwim-end-of-code-or-line
                                          mwim-end-of-line-or-code))
                 (insert (ivy-cleanup-string ivy-text))
                 (when (memq this-command '(ivy-delete-char delete-forward-char))
                   (beginning-of-line)))
               (setq my-ivy-fly--travel t)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (let* ((kbd (kbd "M-n"))
               (cmd (key-binding kbd))
               (future (and cmd
                            (with-temp-buffer
                              (when (ignore-errors
                                      (call-interactively cmd) t)
                                (buffer-string))))))
          (when future
            (save-excursion
              (insert (propertize (replace-regexp-in-string
                                   "\\\\_<" ""
                                   (replace-regexp-in-string
                                    "\\\\_>" ""
                                    future))
                                  'face 'shadow)))
            (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))



    ;; Enhance M-x
    (use-package amx
      :init (setq amx-history-length 20))

    ;; Better sorting and filtering
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
            '((counsel-rg . ivy-prescient-non-fuzzy)
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

    ;; Additional key bindings for Ivy
    (use-package ivy-hydra
      :commands ivy-hydra-read-action
      :init (setq ivy-read-action-function #'ivy-hydra-read-action))

    ;; Integrate yasnippet
    (use-package ivy-yasnippet
      :bind ("C-c C-y" . ivy-yasnippet))


    ;; Quick launch apps
    (bind-key "s-<f6>" #'counsel-linux-app counsel-mode-map)


    ;; Support pinyin in Ivy
    ;; Input prefix ':' to match pinyin
    ;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
    ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
    (use-package pinyinlib
      :commands pinyinlib-build-regexp-string
      :init
      (with-no-warnings
        (defun ivy--regex-pinyin (str)
          "The regex builder wrapper to support pinyin."
          (or (pinyin-to-utf8 str)
              (and (fboundp 'ivy-prescient-non-fuzzy)
                   (ivy-prescient-non-fuzzy str))
              (ivy--regex-plus str)))

        (defun my-pinyinlib-build-regexp-string (str)
          "Build a pinyin regexp sequence from STR."
          (cond ((equal str ".*") ".*")
                (t (pinyinlib-build-regexp-string str t))))

        (defun my-pinyin-regexp-helper (str)
          "Construct pinyin regexp for STR."
          (cond ((equal str " ") ".*")
                ((equal str "") nil)
                (t str)))

        (defun pinyin-to-utf8 (str)
          "Convert STR to UTF-8."
          (cond ((equal 0 (length str)) nil)
                ((equal (substring str 0 1) "!")
                 (mapconcat
                  #'my-pinyinlib-build-regexp-string
                  (remove nil (mapcar
                               #'my-pinyin-regexp-helper
                               (split-string
                                (replace-regexp-in-string "!" "" str )
                                "")))
                  ""))
                (t nil)))

        (mapcar
         (lambda (item)
           (let ((key (car item))
                 (value (cdr item)))
             (when (member value '(ivy-prescient-fuzzy-non
                                   ivy--plus-regex))
               (setf (alist-get key ivy-re-builders-alist)
                     #'ivy--regex-pinyin))))
         ivy-re-builders-alist)))))


;; Complete everything
(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("M-/" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0.1
        company-show-numbers t
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
		;; Forbid company mode in some mode.
        company-global-modes '(not erc-mode message-mode help-mode gud-mode shell-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

(provide 'init-company)

;;;;;init-company.el ends here
