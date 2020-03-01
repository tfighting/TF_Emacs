;; init-function.el --- Define functions.	-*- lexical-binding: t -*-

;;

;;; Commentary:
;;
;; Define some neccssary functions.
;;

;;; Code:

(eval-when-compile
  (require 'init-constant))

;;Check whether the font is installed.
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(global-set-key (kbd "C-z R") #'revert-this-buffer)

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

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (message "WARNING: Current buffer is not attached to a file!")))



;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load-file user-init-file))
(bind-key "C-c C-l" #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of T_Fighting Emacs."
  (interactive)
  (browse-url *t_fighting-homepage*))
;; Open custom file
(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example
         (expand-file-name "custom-example.el" user-emacs-directory)))
    (unless (file-exists-p custom-file)
      (if (file-exists-p custom-example)
          (copy-file custom-file)
        (error "Unable to find \"%s\"" custom-example)))
    (find-file custom-file)))


;;Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

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


(define-minor-mode t_fighting-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group t_fighting
  (if t_fighting-read-mode
      (progn
        (when (fboundp 'olivetti-mode)
          (olivetti-mode 1))
        (when (fboundp 'mixed-pitch-mode)
          (mixed-pitch-mode 1)))
    (progn
      (when (fboundp 'olivetti-mode)
        (olivetti-mode -1))
      (when (fboundp 'mixed-pitch-mode)
        (mixed-pitch-mode -1)))))
(global-set-key (kbd "M-<f7>") #'t_fighting-read-mode)

;; Update
;;

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

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist." dir))))
(defalias 't_fighting-update-org 'update-org)

;;
;;python
;;

;; Run the current python file.
(defun python/run-current-file (&optional directory)
  "Execute the current python file."
  (interactive
   (list (or (and current-prefix-arg
                  (read-directory-name "Run in directory: " nil nil t))
             default-directory)))
  (when (buffer-file-name)
    (let* ((command (or (and (boundp 'executable-command) executable-command)
                        (concat "python3 " (buffer-file-name))))
           (default-directory directory)
           (compilation-ask-about-save nil))
      (executable-interpret (read-shell-command "Run: " command)))))

(with-eval-after-load 'python
  (define-key python-mode-map [f4] 'python/run-current-file))

;; Stop the currnet python file running.
(defun stop-python-file-running ()
  "Stop the current python file."
  (interactive)
  (if (get-buffer "*interpretation*")
      (kill-buffer "*interpretation*")
    (message (format "The buffer %s doesn't exist!" "*interpretation*"))))
(global-set-key (kbd "C-<f4>") 'stop-python-file-running)


(buffer-live-p "*interpretation*")

;; Jump to end and newline.
(defun jump-to-newline ()
  "Jump to the next line."
  (interactive)
  (mwim-end-of-code-or-line)
  (newline))
(global-set-key (kbd "C-j") 'jump-to-newline)

(provide 'init-functions)

;;the init-funcs.el ends here.
(get-buffer "*interpretation*")