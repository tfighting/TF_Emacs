;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package lsp-mode
  :hook (python-mode . lsp-deferred)
  :commands lsp
  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp--describe-thing-at-point))

  :custom
  (lsp-auto-guess-root t)  ;; detect project root
  (lsp-prefer-flymake nil)    ;; use flycheck
  (flymake-fringe-indicator-position 'right-fringe))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-eldoc-enable-hover nil) ; Disable eldoc displays in minibuffer

  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                         ,(face-foreground 'font-lock-string-face)
                         ,(face-foreground 'font-lock-constant-face)
                         ,(face-foreground 'font-lock-variable-name-face)))

  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-ignore-duplicate t)

  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defun my-lsp-ui-imenu-hide-mode-line ()
    "Hide the mode-line in lsp-ui-imenu."
    (setq mode-line-format nil))
  (advice-add #'lsp-ui-imenu :after #'my-lsp-ui-imenu-hide-mode-line))


;; Completion
(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto)
  :config
  ;; WORKAROUND:Fix tons of unrelated completion candidates shown
  ;; when a candidate is fulfilled
  ;; @see https://github.com/emacs-lsp/lsp-python-ms/issues/79
  (add-to-list 'company-lsp-filter-candidates '(mspyls))

  (with-no-warnings
    (defun my-company-lsp--on-completion (response prefix)
      "Handle completion RESPONSE.
PREFIX is a string of the prefix when the completion is requested.
Return a list of strings as the completion candidates."
      (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
             (items (cond ((hash-table-p response) (gethash "items" response))
                          ((sequencep response) response)))
             (candidates (mapcar (lambda (item)
                                   (company-lsp--make-candidate item prefix))
                                 (lsp--sort-completions items)))
             (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
             (should-filter (or (eq company-lsp-cache-candidates 'auto)
                                (and (null company-lsp-cache-candidates)
                                     (company-lsp--get-config company-lsp-filter-candidates server-id)))))
        (when (null company-lsp--completion-cache)
          (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
          (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
        (when (eq company-lsp-cache-candidates 'auto)
          ;; Only cache candidates on auto mode. If it's t company caches the
          ;; candidates for us.
          (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
        (if should-filter
            (company-lsp--filter-candidates candidates prefix)
          candidates)))
    (advice-add #'company-lsp--on-completion :override #'my-company-lsp--on-completion)))


;; Ivy integration
(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
         ("C-s-." . lsp-ivy-global-workspace-symbol)))


;; Debug
(use-package dap-mode
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (_args) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (python-mode . (lambda () (require 'dap-python)))))


;; `lsp-mode' and `treemacs' integration
(when emacs/>=25.2p
  (use-package lsp-treemacs
    :after lsp-mode
    :bind (:map lsp-mode-map
           ("C-<f8>" . lsp-treemacs-errors-list)
           ("M-<f8>" . lsp-treemacs-symbols))
    :config
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)))

    (with-no-warnings
	  (when (require 'all-the-icons nil t)
        (treemacs-create-theme "t_fighting-colors"
          :extends "doom-colors"
          :config
          (progn
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
             :extensions (root))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (boolean-data))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (class))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
             :extensions (color-palette))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
             :extensions (constant))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
             :extensions (document))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (enumerator))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (enumitem))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
             :extensions (event))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (field))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
             :extensions (indexer))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
             :extensions (intellisense-keyword))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (interface))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (localvariable))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
             :extensions (method))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (namespace))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
             :extensions (numeric))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
             :extensions (operator))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
             :extensions (property))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
             :extensions (snippet))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
             :extensions (string))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (structure))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
             :extensions (template))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
             :extensions (collapsed) :fallback "+")
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
             :extensions (expanded) :fallback "-")
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (classfile))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
             :extensions (default-folder-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
             :extensions (default-folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (default-root-folder-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
             :extensions (default-root-folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions ("class"))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (file-type-jar))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (folder-open))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
             :extensions (folder-type-component-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
             :extensions (folder-type-component))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (folder-type-library-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
             :extensions (folder-type-library))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
             :extensions (folder-type-maven-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
             :extensions (folder-type-maven))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
             :extensions (folder-type-package-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
             :extensions (folder-type-package))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-create))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-flat))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
             :extensions (icon-hierarchical))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-link))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-refresh))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-unlink))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
             :extensions (jar))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (library))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
             :extensions (packagefolder-open))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (packagefolder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (package))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
             :extensions (java-project))))

        (setq lsp-treemacs-theme "centaur-colors")))))


;; Microsoft python-language-server support
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))


(provide 'init-lsp)
