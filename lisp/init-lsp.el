;;; init-lsp.el --- Lsp Confoguration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Lsp


(require 'nox)
(add-hook 'python-mode-hook '(lambda () (nox-ensure)))


(use-package posframe
	:demand t)

(provide 'init-lsp)

;;; init-lsp.el ends here.


