;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Auto-completion configurations.
;;

;;; Code:

;;; Commentary:
;;
;; Auto-completion configurations.
;;

;;; Code:


(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :bind(:map company-active-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next)
        ("<tab>" . company-complete-common-or-cycle))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations 't)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))
;; -ComPac

;; CompanyLSPPac
(use-package company-lsp
  :defer t
  :custom (company-lsp-cache-candidates 'auto))
;; -CompanyLSPPac

;; CompanyTabNinePac
(use-package company-tabnine
  :demand
  :custom
  (company-tabnine-max-num-results 5)
  :bind
  (("M-q" . company-other-backend)
   ("C-<f12>" . company-tabnine))
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  (add-hook 'lsp-after-open-hook
            (lambda ()
              (add-to-list 'company-transformers 'company//sort-by-tabnine t)
              (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))))
;; -CompanyTabNinePac


(provide 'init-company)


;;init-company.el ends here
