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
     :hook (python-mode . lsp-deferred)
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

     :init (setq lsp-ui-doc-enable nil
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


   ;; `lsp-mode' and `treemacs' integration.
   (when emacs/>=25.2p
     (use-package lsp-treemacs
       :bind (("C-<f8>" . lsp-treemacs-errors-list)
              ("M-<f8>" . lsp-treemacs-symbols)
              ("s-<f8>" . lsp-treemacs-java-deps-list))
       :config
       (with-eval-after-load 'ace-window
         (when (boundp 'aw-ignored-buffers)
           (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
           (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers)))

       (with-no-warnings
	     (when (require 'all-the-icons nil t)
           (treemacs-create-theme "t_fighting-colors"
                                  :extends "doom-colors"
                                  :config
                                  (progn
                                    (treemacs-create-icon
                                     :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.1 :v-adjust -0.1 :face 'all-the-icons-blue))
                                     :extensions (root))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'all-the-icons-lblue))
                                     :extensions (boolean-data))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "settings_input_component" :face 'all-the-icons-orange))
                                     :extensions (class))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "palette"))
                                     :extensions (color-palette))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "square-o"))
                                     :extensions (constant))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "file-text-o"))
                                     :extensions (document))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "storage" :face 'all-the-icons-orange))
                                     :extensions (enumerator))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "format_align_right" :face 'all-the-icons-lblue))
                                     :extensions (enumitem))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "bolt" :face 'all-the-icons-orange))
                                     :extensions (event))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'all-the-icons-lblue))
                                     :extensions (field))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "search"))
                                     :extensions (indexer))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "filter_center_focus"))
                                     :extensions (intellisense-keyword))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "share" :face 'all-the-icons-lblue))
                                     :extensions (interface))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "tag" :height 1.0 :v-adjust 0 :face 'all-the-icons-blue))
                                     :extensions (localvariable))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "cube" :face 'all-the-icons-purple))
                                     :extensions (method))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "view_module" :face 'all-the-icons-lblue))
                                     :extensions (namespace))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "format_list_numbered"))
                                     :extensions (numeric))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
                                     :extensions (operator))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "wrench"))
                                     :extensions (property))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "format_align_center"))
                                     :extensions (snippet))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "text-width"))
                                     :extensions (string))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "settings_input_component" :face 'all-the-icons-orange))
                                     :extensions (structure))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "format_align_center"))
                                     :extensions (template))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                                     :extensions (collapsed) :fallback "+")
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                                     :extensions (expanded) :fallback "-")
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face 'font-lock-doc-face))
                                     :extensions (classfile))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'all-the-icons-blue))
                                     :extensions (default-folder-opened))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'all-the-icons-blue))
                                     :extensions (default-folder))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'all-the-icons-green))
                                     :extensions (default-root-folder-opened))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'all-the-icons-green))
                                     :extensions (default-root-folder))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face 'font-lock-doc-face))
                                     :extensions ("class"))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-zip" :face 'font-lock-doc-face))
                                     :extensions (file-type-jar))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'font-lock-doc-face))
                                     :extensions (folder-open))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face))
                                     :extensions (folder))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'all-the-icons-orange))
                                     :extensions (folder-type-component-opened))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'all-the-icons-orange))
                                     :extensions (folder-type-component))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'all-the-icons-green))
                                     :extensions (folder-type-library-opened))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'all-the-icons-green))
                                     :extensions (folder-type-library))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'all-the-icons-pink))
                                     :extensions (folder-type-maven-opened))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'all-the-icons-pink))
                                     :extensions (folder-type-maven))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'font-lock-type-face))
                                     :extensions (folder-type-package-opened))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-type-face))
                                     :extensions (folder-type-package))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "plus" :face 'font-lock-doc-face))
                                     :extensions (icon-create))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "list" :face 'font-lock-doc-face))
                                     :extensions (icon-flat))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-material "share" :face 'all-the-icons-lblue))
                                     :extensions (icon-hierarchical))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "link" :face 'font-lock-doc-face))
                                     :extensions (icon-link))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "refresh" :face 'font-lock-doc-face))
                                     :extensions (icon-refresh))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "chain-broken" :face 'font-lock-doc-face))
                                     :extensions (icon-unlink))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-alltheicon "java" :face 'all-the-icons-orange))
                                     :extensions (jar))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "book" :v-adjust 0 :face 'all-the-icons-green))
                                     :extensions (library))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
                                     :extensions (packagefolder-open))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'all-the-icons-lblue))
                                     :extensions (packagefolder))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "package" :v-adjust 0 :face 'font-lock-doc-face))
                                     :extensions (package))
                                    (treemacs-create-icon
                                     :icon (format "%s\t" (all-the-icons-octicon "repo" :height 1.1 :v-adjust -0.1 :face 'all-the-icons-blue))
                                     :extensions (java-project))))

           (setq lsp-treemacs-theme "t_fighting-colors")))))



   ;; Microsoft python-language-server support
   (use-package lsp-python-ms
     :hook (python-mode . (lambda ()
                            (require 'lsp-python-ms)
                            (lsp-deferred))))

   ))

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
