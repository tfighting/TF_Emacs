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
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point))

  :custom
  (lsp-auto-guess-root t)  ;; detect project root
  (lsp-prefer-flymake nil)    ;; use flycheck
  (read-process-output-max (* 1024 1024))
  (lsp-file-watch-threshold 2000)
  (lsp-keep-workspace-alive nil) ;; close lsp server after last workspace closed
  )


(use-package lsp-ui
  :after lsp-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         ("M-<f6> " . flycheck-list-errors)
         :map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-enable nil      ;; close text display
        lsp-eldoc-enable-hover nil ;; Disable eldoc displays in minibuffer
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))
        )

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

;; Microsoft python-language-server support
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))


(provide 'init-lsp)

;;; init-lsp.el ends here.
