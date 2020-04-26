;;; init-fuctions.el --- Helper Fuctions -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Fuctions


;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

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

;;
;; Misc
;;
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

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


;; Mark to everywhere.
(defun t_fighting-mark-to-char ()
  (interactive)
  (call-interactively #'set-mark-command)
  (call-interactively #'avy-goto-char))
(global-set-key (kbd "C-z m") 't_fighting-mark-to-char)


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
