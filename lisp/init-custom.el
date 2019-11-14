;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))


(defgroup t_fighting nil
  "Centaur Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/tfighting/TF_Emacs"))

(defcustom t_fighting-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set Centaur logo. nil means official logo."
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



(defcustom t_fighting-theme 'default
  "Set color theme."
  :group 't_fighting
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "colorful theme" colorful)
          (const :tag "Classic theme" classic)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Day theme" day)
          (const :tag "night theme" night)
          symbol))

(defcustom t_fighting-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 't_fighting
  :type 'boolean)

(defcustom t_fighting-lsp 'lsp-mode
  "Set language server."
  :group 't_fighting
  :type '(choice
          (const :tag "LSP Mode" 'lsp-mode)
          (const :tag "eglot" 'eglot)
          nil))


;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

(provide 'init-custom)


;;; init-custom.el ends here
