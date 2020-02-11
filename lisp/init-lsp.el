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
  (flymake-fringe-indicator-position 'right-fringe))


(use-package lsp-ui
  :custom-face
  (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket")
    :color amaranth :quit-key "q")
   ("Doc"
    (("d e" (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)) "enable" :toggle lsp-ui-doc-enable)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)) "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top) "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom) "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point) "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d f" (setq lsp-ui-doc-alignment 'frame) "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window) "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)) "enable" :toggle lsp-ui-sideline-enable)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover)) "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics)) "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)) "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))"code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate)) "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-enable nil
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'bottom
              lsp-ui-doc-border (face-foreground 'default)
              lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

              lsp-ui-imenu-enable t
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face))

              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-ignore-duplicate t)
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
