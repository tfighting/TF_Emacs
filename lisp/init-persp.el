;;; init-persp.el --- Initialize perspectives configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; perspectives configurations.
;;

;;; Code:


(eval-when-compile
  (require 'init-custom))

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines ivy-sort-functions-alist
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (window-setup . toggle-frame-maximized))
  :init
  (setq persp-keymap-prefix (kbd "C-x p")
        persp-nil-name "default"
        persp-set-last-persp-for-new-frames nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-auto-resume-time (if t_fighting-dashboard 0 1.0)
        persp-common-buffer-filter-functions
        (list #'(lambda (b)
                  "Ignore temporary buffers."
                  (or (string-prefix-p " " (buffer-name b))
                      (and (string-prefix-p "*" (buffer-name b))
                           (not (string-equal "*scratch*" (buffer-name b))))
                      (string-prefix-p "magit" (buffer-name b))
                      (string-prefix-p "Pfuture-Callback" (buffer-name b))
                      (eq (buffer-local-value 'major-mode b) 'nov-mode)
                      (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))
  :config
  ;; Integrate IVY
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))))

;; Integrate `projectile'
(use-package persp-mode-projectile-bridge
  :after projectile
  :functions (persp-get-by-name
              persp-add-new
              set-persp-parameter
              persp-add-buffer
              my-persp-mode-projectile-bridge-add-new-persp)
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives
             persp-mode-projectile-bridge-add-new-persp
             projectile-project-buffers)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  ;; HACK: Allow saving to files
  (eval-and-compile
    (defun my-persp-mode-projectile-bridge-add-new-persp (name)
      (let ((persp (persp-get-by-name name *persp-hash* :nil)))
        (if (eq :nil persp)
            (prog1
                (setq persp (persp-add-new name))
              (when persp
                (set-persp-parameter 'persp-mode-projectile-bridge t persp)
                (persp-add-buffer (projectile-project-buffers)
                                  persp nil nil)))
          persp)))
    (advice-add #'persp-mode-projectile-bridge-add-new-persp
                :override #'my-persp-mode-projectile-bridge-add-new-persp)))

(provide 'init-persp)


;;; init-persp.el ends here
