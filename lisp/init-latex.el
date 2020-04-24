;;; init-latex.el --- latex -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: LaTeX

(use-package auctex)
(use-package cdlatex
  :init
  (setq outline-minor-mode-prefix [(control o)])
  (setq TeX-parse-selt t)

  ;; Search via pdf
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
				'(("Sumatra PDF" ("\"D:/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o"))))

	:bind
  ("C-c C-p d" . TeX-clean)
  :hook
  (LaTeX-mode-hook . (lambda ()
											 (turn-on-cdlatex)
											 (outline-minor-mode)
											 (turn-on-reftex)
											 (auto-fill-mode)
											 (reftex-isearch-minor-mode)
											 (TeX-fold-mode t)
											 (outline-hide-body)
											 (assq-delete-all (quote output-pdf) TeX-view-program-selection)
											 (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))
	:config
	;; match parentheses.
	(defun match-paren (arg)
		"Go to the matching paren if on a paren; otherwise insert %."
		(interactive "p")
		(cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
					((looking-at "\\s\)") (forward-char 1) (backward-list 1))
					(t (self-insert-command (or arg 1)))))
	(global-set-key "%" 'match-paren)

	;; keep better completion
	(setq hippie-expand-try-functions-list
				'(try-expand-dabbrev
					try-expand-dabbrev-visible
					try-expand-dabbrev-all-buffers
					try-expand-dabbrev-from-kill
					try-complete-file-name-partially
					try-complete-file-name
					try-expand-all-abbrevs
					try-expand-list
					try-expand-line
					try-complete-lisp-symbol-partially
					try-complete-lisp-symbol)))


(use-package org-edit-latex
  :after org)

(provide 'init-latex)

;;; init-latex.el ends here.
