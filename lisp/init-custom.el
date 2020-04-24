;;; init-custom.el --- Customize some variables -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <T_fig@DESKTOP-ALBLEA4>
;; Keywords: variables


;; Persional information.
(defcustom t_fighting-homepage "https://github.com/tfighting/win-emacs"
  "T_fighting-homepage."
  :group 't_fighting
  :type 'string )

(defcustom t_fighting-logo (expand-file-name "images/KEC_Dark_BK.png" user-emacs-directory)
  "Set Dashboard logo. nil means official logo."
  :group 't_fighting
  :type 'string)

(defcustom t_fighting-full-name "T_Fighting"
  "Set user full name."
  :group 't_fighting
  :type 'string)

(defcustom t_fighting-mail-address "545298210@qq.com"
  "Set user email address."
  :group 't_fighting
  :type 'string)

(provide 'init-custom)

;;; init-custom.el ends here
