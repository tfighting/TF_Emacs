;; init-awesome-tab.el --- Better default configurations.	-*- lexical-binding: t -*-

;;
;; Commentary:
;;
;; Code:


(eval-when-compile
  (require 'init-const))

(use-package awesome-tab
  :load-path (lambda () (expand-file-name "site-lisp/awesome-tab" user-emacs-directory))
  :hook (after-init . awesome-tab-mode)
  :init
  (when *sys/gui*
    (require 'all-the-icons))
  :pretty-hydra
  ((:title (pretty-hydra-title "Awesome-Tab" 'octicon "fold")
    :foreign-keys warn :quit-key "q")
   ("Switch Buffer"
    (("t j" awesome-tab-ace-jump "jump selected buffer" :exit t)
     ("t l" awesome-tab-move-current-tab-to-left "move to left" :exit nil)
     ("t r" awesome-tab-move-current-tab-to-right "move to right" :exit nil)
     ("t f" awesome-tab-forward "forward" :exit nil)
     ("t b" awesome-tab-backward "backward" :exit nil)
     ("w f" awesome-tab-forward-tab-other-window "forward other window" :exit nil)
     ("w b" awesome-tab-backward-tab-other-window "backward other window" :exit nil))

    "Switch Group"
    (("b g" awesome-tab-backward-group "backward group" :exit nil)
     ("b f" awesome-tab-forward-group "forward group" :exit nil)
     ("t g" awesome-tab-switch-group "switch group" :exit nil))

    "Kill Buffer"
    (("k c" kill-current-buffer "kill current buffer" :exit t)
     ("k o" awesome-tab-kill-other-buffers-in-current-group "kill other buffer" :exit t)
     ("k a" awesome-tab-kill-all-buffers-in-current-group "kill all buffers" :exit t))))
  :bind (("C-<f7>" . awesome-tab-hydra/body)
         ("<f7>" . awesome-tab-ace-jump)))

(provide 'init-awesome-tab)
;;;init-awesome-tab.el ends here.
