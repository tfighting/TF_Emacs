;;; init-hydra.el --- Hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: hydara

(use-package pretty-hydra
	:demand t
	:init
	(cl-defun pretty-hydra-title (title &key face height v-adjust)
    "Add an  hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
			(propertize title 'face face))))



(provide 'init-hydra)

;;; init-hydra.el ends here.
