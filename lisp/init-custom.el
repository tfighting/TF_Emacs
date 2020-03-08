;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant))

;; Persional information.
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

(defcustom t_fighting-server t
  "Enable `server-mode' or not."
  :group 't_fighting
  :type 'boolean)

(defcustom t_fighting-load-theme t
  "Enable `load-theme' or not."
  :group 't_fighting
  :type 'boolean)

(defcustom t_fighting-display-icons t
  "Whether to display icons."
  :group 't_fighting
  :type 'boolean)

(defcustom t_fighting-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 't_fighting
  :type 'boolean)

;; Load 'custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;; init-custom.el ends here
