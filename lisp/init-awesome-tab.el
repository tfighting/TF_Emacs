 ;;; init-awesome-tab.el --- Better toggle tab Configuration.	-*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;
;; Better toggle tab Configuration.
;;


;;; Code:

;;Some configuration can be modified in ~/.emacs.d/site-lisp/awesome-tab/awesome-tab.el
(use-package awesome-tab
  :ensure nil
  :init
  (require 'awesome-tab)
  (when *sys/gui*
    (require 'all-the-icons))

  :config
  (awesome-tab-mode t)

  ;; Whether to display icons.
  (setq awesome-tab-display-icon (and *sys/gui* t_fighting-display-icons)
        awesome-tab-show-tab-index *sys/gui*)
  ;; Use s-n switch tab.
  (global-set-key (kbd "M-1") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-2") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-3") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-4") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-5") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-6") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-7") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-8") 'awesome-tab-select-visible-tab)
  :pretty-hydra
  ((:title (pretty-hydra-title "Awesome-Tabs" 'wicon "day-sunny")
    :foreign-keys warn :quit-key "q")
   ("Window"
    (("f" awesome-tab-forward-tab-other-window "window forward")
     ("b" awesome-tab-backward-tab-other-window "window backward"))

    "Kill"
    (("o" awesome-tab-kill-other-buffers-in-current-group "kill other" :exit t)
     ("a" awesome-tab-kill-all-buffers-in-current-group "kill all"))

    "Misc"
    (("s" awesome-tab-switch-group "switch group")
     ("j" awesome-tab-ace-jump "jump tab" :exit t))))

  :bind
  ("C-<f7>" . awesome-tab-hydra/body)
  ("<f7>" . awesome-tab-ace-jump))


(provide 'init-awesome-tab)
;;; init-awesome-tab.el ends here.
