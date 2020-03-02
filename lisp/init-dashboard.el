;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Dashboard configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant)
  (require 'init-custom))

;; Dashboard
(when t_fighting-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold t)))))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("R" . restore-session)
           ("L" . persp-load-state-from-file)
           ("S" . open-custom-file)
           ("U" . update-all-packages)
           ("q" . quit-dashboard))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
    :init (dashboard-setup-startup-hook)
    :config
    (setq dashboard-banner-logo-title "T_Fighting EMACS - STAY HUNGRY & STAY FOOLISH"
          dashboard-startup-banner (or t_fighting-logo 'official)
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5))

          dashboard-set-init-info t
          dashboard-set-file-icons t_fighting-display-icons
          dashboard-set-heading-icons t_fighting-display-icons
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "file-directory")
                                    (registers . "database"))

          dashboard-set-footer t ;; wheather to display footer
          dashboard-footer-message (format "T_Fighting, %s" (format-time-string "%Y"))
          dashboard-footer-icon (
                                 cond ( (and *sys/gui* t_fighting-display-icons)
                                        (all-the-icons-faicon "apple"
                                                              :height 1.1
                                                              :v-adjust -0.05
                                                              :face 'error))
                                 ((char-displayable-p ?☕) "☕ ")
                                 (t (propertize ">" 'face 'font-lock-doc-face)))

          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,(when (and *sys/gui* t_fighting-display-icons)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage"
              "Browse homepage"
              (lambda (&rest _) (browse-url *t_fighting-homepage*)))
             ;; display previous session
             (,(when (and *sys/gui* t_fighting-display-icons)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore"
              "Restore previous session"
              (lambda (&rest _) (restore-session)))

             ;; display custom file
             (,(when (and *sys/gui* t_fighting-display-icons)
                 (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
              "Settings"
              "Open custom file"
              (lambda (&rest _) (find-file custom-file)))

             ;; display update packages
             (,(when (and *sys/gui* t_fighting-display-icons)
                 (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
              "Update"
              "Update T_fighting Emacs"
              (lambda (&rest _) (t_fighting-update-all-packages)))
             )))

    (defun my-banner-path (&rest _)
      "Return the full path to banner."
      (expand-file-name "banner.txt" user-emacs-directory))
    (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (quit-window t)
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))
        (message "Done")))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))

    (defun dashboard-goto-agenda ()
      "Go to agenda."
      (interactive)
      (funcall (local-key-binding "a")))))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
