;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Window configurations.
;;

;;; Code:


;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :bind (("C-z r" . winner-redo)
         ("C-z u" . winner-undo))
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Quickly switch windows
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :config
  (ace-window-display-mode t)
  (add-hook 'window-setup-hook #'toggle-frame-maximized)
  ;; Select widnow via `s-1'...`s-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number (string-to-number (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "s-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))



(provide 'init-window)
;;; init-window.el ends here
