;;; init-eaf.el --- -*- lexical-binding: t -*-


;;
;; Commentary:
;;
;; The file initializes Emacs Application Framework
;;
;; Code
;;

(eval-when-compile
  (require 'init-const))

(use-package eaf
  :ensure nil
  :init
  (require 'eaf)
  :if *sys/gui*
  :custom
  (eaf-find-alternate-file-in-dired t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser.
  :config
  (eaf-setq eaf-browser-default-zoom "1.0")
  (defalias 'browse-web #'eaf-open-browser)
  ;; Use your own github token,it's free generate at https://github.com/setting/tokens
  ;; Setting token will avoid block off by github API times limit.
  (setq eaf-grip-token "ee798c33207366b0000c0069d07e4812e04b897b")
  ;; You need configuration your own local proxy program first.
  ;; (setq eaf-proxy-type "socks5")
  ;; (setq eaf-proxy-host "127.0.0.1")
  ;; (setq eaf-proxy-port "1080")

  ;; some keybindings about browser
  (eaf-bind-key undo_action "C-/" eaf-browser-keybinding)
  (eaf-bind-key redo_action "C-?" eaf-browser-keybinding)
  (eaf-bind-key scroll_up_page "M-n" eaf-browser-keybinding)
  (eaf-bind-key scroll_down_page "M-p" eaf-browser-keybinding)
  (eaf-bind-key scroll_to_begin "M->" eaf-browser-keybinding)
  (eaf-bind-key scroll_to_bottom "M-<" eaf-browser-keybinding)
  (eaf-bind-key open_link "M-h" eaf-browser-keybinding)
  (eaf-bind-key open_link_new_buffer "M-H" eaf-browser-keybinding))



(provide 'init-eaf)
;;; init-eaf.el ends here.
