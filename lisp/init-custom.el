;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Unbind unnecessary keys
(global-set-key (kbd "C-z") nil)
;; Move up/down paragraph
(global-set-key (kbd "s-n") #'forward-paragraph)
(global-set-key (kbd "s-p") #'backward-paragraph)
;; Persional information
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

(defcustom t_fighting-theme-alist
  '((default  . doom-one)
    (classic  . doom-molokai)
    (colorful . doom-snazzy)
    (dark     . doom-dark+)
    (light    . doom-one-light)
    (day      . doom-acario-light)
    (night    . doom-city-lights))
  "The color theme list."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Theme name")
                :value-type (symbol :tag "Internal theme name")))


(defcustom t_fighting-theme 'default
  "Set color theme."
  :group 'centaur
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    t_fighting-theme-alist)
                 symbol))


(defcustom t_fighting-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 't_fighting
  :type 'boolean)

(defcustom t_fighting-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬))
  "Alist of symbol prettifications."
  :group 't_fighting
  :type '(alist :key-type string :value-type (choice character sexp)))


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
