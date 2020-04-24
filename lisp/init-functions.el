;;; init-fuctions.el --- Helper Fuctions -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Fuctions


(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(global-set-key (kbd "C-x K") #'delete-this-file)

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; revert the current file.
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(global-set-key (kbd "s-r") #'revert-this-buffer)

;; Open files via externaly applications
(defun open-in-external-app ()
	"Open files via externaly applications."
	(interactive)
	(let* ((file-path (if (derived-mode-p 'dired-mode)
												(dired-get-file-for-visit)
											(buffer-file-name))))

		(if (string-match "\\(?:\\.\\(?:md\\|pdf\\)\\)" file-path)
				(w32-shell-execute "open" file-path))))

(with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "<C-return>") 'open-in-external-app ))


;; Update packages
(defun update-all-packages ()
  "Update all packages right now"
  (interactive)
  (message "Updating all packages....")
  (use-package auto-package-update
    :if (not (daemonp))
    :custom
    (auto-package-update-delete-old-versions t)
    (auto-package-update-hide-results nil)
    :init
    (auto-package-update-now))
  (message "Updating all packages done!"))
(defalias 't_fighting-update-all-packages 'update-all-packages)

;;
;;python
;;

(defun python-run-current-file ()
  "Execute the current python file."
  (interactive)
	(python-shell-send-file buffer-file-name))

(defun python-quit-interpreter ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-quit-subjob)
  (kill-buffer-and-window))

(defun python-interrupt-interpreter ()
  (interactive)
	(let ((file (file-name-nondirectory buffer-file-name)))
  (switch-to-buffer "*Python*")
  (comint-interrupt-subjob)
  (switch-to-buffer file)))



(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-,")  'python-run-current-file)
	(define-key python-mode-map (kbd "C-c C-q") 'python-quit-interpreter)
	(define-key python-mode-map (kbd "C-c C-k") 'python-interrupt-interpreter))



;; Jump to end and newline.
(defun jump-to-newline ()
  "Jump to the next line."
  (interactive)
  (call-interactively  #'move-end-of-line)
  (call-interactively #'newline))
(global-set-key (kbd "<M-f2>") 'jump-to-newline)

;;
;; Org
;;

(defun insert-template ()
	"Insert keywords template"
	(interactive)
	(let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end)))
    (insert "#+")
		(complete-symbol text)))

(with-eval-after-load 'org
	(define-key org-mode-map (kbd "C-c k") 'insert-template))

(provide 'init-functions)

;;; init-functions ends here.
