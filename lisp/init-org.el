;;; init-org.el --- org configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;; Keywords: Org

(use-package org
  :ensure nil
  :custom
  (org-tags-column -80)
  (org-catch-invisible-edits 'smart)
  (org-startup-indented t) ;; auto indent.
  (org-log-done 'time) ;; display DONE time
  (org-src-fontify-natively t) ;;synax highlight
  (org-confirm-babel-evaluate nil) ;; don't ask to comfirm
  (org-src-tab-acts-natively t)

  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCEL(c)")))

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
  ((:title (pretty-hydra-title "Org Tempate")
		   :color teal :quit-key "q")
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
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)
   :map org-mode-map
   ("C-c t" . (lambda ()
				"Insert org template."
				(interactive)
				(if (or (region-active-p) (looking-back "^\s*" 1))
					(org-hydra/body)
				  (self-insert-command 1)))))

  :hook ((org-mode . (lambda () (setq truncate-lines nil)))
		 (org-mode . (lambda()
                       (diminish 'org-indent-mode)
                       ;; WORKAROUND: Prevent text moving around while using brackets
                       ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                       (make-variable-buffer-local 'show-paren-mode)
                       (setq show-paren-mode nil))))

  :config
  (require 'org-tempo)
  (when (file-directory-p "~/org/agenda/")
	(setq org-agenda-files (list "~/org/agenda/")))

  ;; Code block suport run python file.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (emacs-lisp . t)))

  ;; switch markdown
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)
  ;; (with-eval-after-load 'counsel
  ;;   (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Bind f6 to org-gfm-export-to-markdown.
  (with-eval-after-load 'org
	(define-key org-mode-map (kbd "<f6>") 'org-gfm-export-to-markdown ))

  ;; Generate table of contents
  (use-package toc-org
	:hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
	:diminish)

  (use-package org-bullets
	:if (char-displayable-p ?◉)
	:init (setq org-bullets-bullet-list '("◉" "◉" "◉" "◉"))
	:hook (org-mode . org-bullets-mode))

  ;; Pomodoro
  (use-package org-pomodoro
	:custom-face
	(org-pomodoro-mode-line ((t (:inherit warning))))
	(org-pomodoro-mode-line-overtime ((t (:inherit error))))
	(org-pomodoro-mode-line-break ((t (:inherit success))))
	:bind (:map org-agenda-mode-map
				("P" . org-pomodoro))))





(provide 'init-org)

;;; init-org.el ends here.
