;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package org
  :ensure nil
  :custom-face
  (org-ellipsis ((t (:foreground nil))))

  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :custom
  (org-tags-column -80)
  (org-catch-invisible-edits 'smart)
  (org-startup-indented t)
  (org-ellipsis (if (char-displayable-p ?) "  " nil))
  (org-log-done 'time)
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  ;;highlight code
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-pretty-entities nil)
  (org-hide-emphasis-markers t)
  (org-agenda-window-setup 'other-window)

  (org-todo-keywords
   '((sequence "TODO(t)" "PROCESS(p)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")))
  (org-todo-keyword-faces '(("HANGUP" . warning)))
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)))

  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("p" (hot-expand "<s" "python :results output") "python")
     ("y" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))

  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))

  :hook ((org-mode . (lambda ()
                       "Beautify org symbols."
                       (push '("[ ]" . ?☐) prettify-symbols-alist)
                       (push '("[X]" . ?☑) prettify-symbols-alist)
                       (push '("[-]" . ?⛝) prettify-symbols-alist)

                       (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
                       (push '("#+END_SRC" . ?□) prettify-symbols-alist)
                       (push '("#+BEGIN_QUOTE" . ?») prettify-symbols-alist)
                       (push '("#+END_QUOTE" . ?«) prettify-symbols-alist)
                       (push '("#+HEADERS" . ?☰) prettify-symbols-alist)
                       (push '("#+RESULTS:" . ?⮮) prettify-symbols-alist)

                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))

  :config
  (set-face-attribute 'org-level-1 nil :height 1.20 :bold t)
  (set-face-attribute 'org-level-2 nil :height 1.15 :bold t)
  (set-face-attribute 'org-level-3 nil :height 1.10 :bold t)
  (set-face-attribute 'org-level-4 nil :height 1.05 :bold t)
  (set-face-attribute 'org-level-5 nil :height 1.05 :bold t)
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))

  ;; switch markdown
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)
  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Generate table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; Pretty UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))


  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
           ("P" . org-pomodoro))))



(provide 'init-org)
;;; init-org.el ends here
