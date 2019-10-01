;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(pcase t_fighting-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish lsp-mode
     :hook (prog-mode . lsp-deferred)
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point))
     :init (setq lsp-auto-guess-root t       ; Detect project root
                 lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
                 flymake-fringe-indicator-position 'right-fringe)
     :config
     ;; Configure LSP clients
     (use-package lsp-clients
       :ensure nil
       :init (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))))

   (use-package lsp-ui
     :functions my-lsp-ui-imenu-hide-mode-line
     :commands lsp-ui-doc-hide
     :custom-face
     (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :bind (:map lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references)
            ("C-c u" . lsp-ui-imenu))
     :init (setq lsp-ui-doc-enable t
                 lsp-ui-doc-use-webkit nil
                 lsp-ui-doc-delay 0.5
                 lsp-ui-doc-include-signature t
                 lsp-ui-doc-position 'top
                 lsp-ui-doc-border (face-foreground 'default)

                 lsp-ui-sideline-enable t
                 lsp-ui-sideline-show-hover nil
                 lsp-ui-sideline-show-diagnostics nil
                 lsp-ui-sideline-ignore-duplicate t

                 lsp-eldoc-enable-hover nil)
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

   (use-package company-lsp
     :init (setq company-lsp-cache-candidates 'auto))

   ;; Debug
   (use-package dap-mode
     :diminish
     :functions dap-hydra/nil
     :bind (:map lsp-mode-map
            ("<f5>" . dap-debug)
            ("M-<f5>" . dap-hydra))
     :hook ((after-init . dap-mode)
            (dap-mode . dap-ui-mode)
            (dap-session-created . (lambda (&_rest) (dap-hydra)))
            (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))

            (python-mode . (lambda () (require 'dap-python)))
            ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))))

   ;; `lsp-mode' and `treemacs' integration.
   (when emacs/>=25.2p
     (use-package lsp-treemacs
       :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list))))

   ;; Microsoft python-language-server support
   (use-package lsp-python-ms
     :hook (python-mode . (lambda ()
                            (require 'lsp-python-ms)
                            (lsp-deferred))))))

(when t_fighting-lsp
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((filename (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (setq buffer-file-name filename)
             (pcase t_fighting-lsp
               ('eglot
                (and (fboundp 'eglot) (eglot)))
               ('lsp-mode
                (and (fboundp 'lsp)
                     ;; `lsp-auto-guess-root' MUST be non-nil.
                     (setq lsp-buffer-uri (lsp--path-to-uri filename))
                     (lsp-deferred))))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      t_fighting-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))

(provide 'init-lsp)


;;; init-lsp.el ends here
