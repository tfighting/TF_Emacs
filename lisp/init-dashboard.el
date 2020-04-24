;;; init-dashboard.el --- dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Dashboard

(eval-when-compile
  (require 'init-custom))

(use-package dashboard
  :diminish (dashboard-mode page-break-lines-mode)
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("S" . open-custom-file)
         ("U" . update-all-packages)
         ("q" . quit-dashboard)
				 :map dashboard-mode-map
				 ("j" . dashboard-next-line)
				 ("k" . dashboard-previous-line))
	:init
	(dashboard-setup-startup-hook)
	:custom
	(dashboard-set-init-info t)
	(dashboard-center-content t)
	(dashboard-show-shortcuts nil)
	(dashboard-banner-logo-title "T_Fighting EMACS - STAY HUNGRY & STAY FOOLISH")
	(dashboard-startup-banner t_fighting-logo)
	(dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (agenda . 5)))

	;;(initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
	(dashboard-set-footer t) ;; wheather to display footer
	(dashboard-footer-message (format "T_Fighting, %s" (format-time-string "%Y")))

	(dashboard-set-navigator t)
	(dashboard-navigator-buttons
   `(((,""
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url t_fighting-homepage)))

			;; display custom file
			(,""
       "Settings"

       "Open custom file"
       (lambda (&rest _) (find-file custom-file)))

			;; display update packages
			(,""
       "Update"
       "Update T_fighting Emacs"
       (lambda (&rest _) (t_fighting-update-all-packages)))
			)))

	:custom-face
  (dashboard-banner-logo-title ((t (:family "Love LetterTW" :height 123))))

	:config

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

	(defun dashboard-goto-bookmarks ()
		"Go to bookmarks."
		(interactive)
		(funcall (local-key-binding "m")))

	(defun dashboard-goto-agenda ()
		"Go to agenda."
		(interactive)
		(funcall (local-key-binding "a"))))


(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
