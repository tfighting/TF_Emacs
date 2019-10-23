;; init-font.el --- Setup fonts.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; (set-default-font "Source Code Pro Semibold 14.5" nil t)
(set-face-attribute 'default nil :font (format "Fira Code Retina-%S" t_fighting-font-size))

(use-package fontify-face)

(provide 'init-fonts)



;;; init-fonts ends here.
