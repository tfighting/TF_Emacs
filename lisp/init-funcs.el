;; init-function.el --- Define functions.	-*- lexical-binding: t -*-

;;

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (text-scale-increase 0)
    (widen)
    (if (and (fboundp 'fancy-narrow-active-p)
             (fancy-narrow-active-p))
        (fancy-widen))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(bind-key "s-r" #'revert-this-buffer)

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
  (browse-url t_fighting-homepage))

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

;;
;;Operate files
;;
(defun t_fighting--file-path ()
  "Retrieve the file path of the current buffer.
Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))


(defun t_fighting/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (t_fighting--file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun t_fighting/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (t_fighting--file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))


;;
;; UI
;;

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun t_fighting--standardize-theme (theme)
  "Standardize THEME."
  (pcase theme
    ('default 'doom-one)
    ('classic 'doom-molokai)
    ('dark 'doom-Iosvkem)
    ('light 'doom-one-light)
    ('daylight 'doom-tomorrow-day)
    (_ (or theme 'doom-one))))

(defun t_fighting-compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name (t_fighting--standardize-theme theme))))

(defun t_fighting-load-theme (theme)
  "Set color THEME."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             '(default classic dark light daylight)))))
  (let ((theme (t_fighting--standardize-theme theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun t_fighting-dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun t_fighting-current-theme ()
  "The current enabled theme."
  (car custom-enabled-themes))

;;
;;python
;;

;; (defun python/run-current-file (&optional directory)
;;   "Execute the current python file."
;;   (interactive
;;    (list (or (and current-prefix-arg
;;                   (read-directory-name "Run in directory: " nil nil t))
;;              default-directory)))
;;   (when (buffer-file-name)
;;     (let* ((command (or (and (boundp 'executable-command) executable-command)
;;                         (concat "python3 " (buffer-file-name))))
;;            (default-directory directory)
;;            (compilation-ask-about-save nil))
;;       (executable-interpret (read-shell-command "Run: " command)))))

;; (with-eval-after-load 'python
;;   (define-key python-mode-map [f4] 'python/run-current-file))

;;display python buffer
(defun display-python-buffer ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer "*Python*")
  (other-window -1))
(with-eval-after-load 'python
  (define-key python-mode-map [f4] 'display-python-buffer))

;;interupt routine
(defun python-interrupt ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-interrupt-subjob)
  (switch-to-prev-buffer))




;;interrupt routine and quit
(defun python-quit ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-quit-subjob)
  (kill-buffer-and-window))




(provide 'init-funcs)
