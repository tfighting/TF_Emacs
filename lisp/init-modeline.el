 ;;; modeline.el --- A simple and useful mode-line Configuration.	-*- lexical-binding: t no-byte-compile: t; -*-

;; Commentary:
;;
;; A simple and useful mode-line Configuration.
;;
;; Code:

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see https://www.gnu.org/software/emacs/manual/html_node/elisp/
;; use setq-default to set it for /all/ modes
;; Keep track of selected window, so we can render the modeline differently

(when (not *sys/gui*)
  (set-face-attribute 'mode-line nil
                      :background "003036"
                      :box '(:color "003036")))
(setq system-time-locale "C")
(setq-default mode-line-format
              (list

               " "
               (propertize "☕" 'face 'font-lock-string-face)

               '(:eval (propertize "%b" 'face 'font-lock-string-face
               'help-echo "buffer-name"))

               ;; Display line and columns
               " " "%02l" ":" "%01c" "   "

               ;; Display time
               "["
               '(:eval (propertize (format-time-string "%H:%M") 'face 'font-lock-type-face))
               "]"
               "["
               (propertize (format-time-string "%a") 'face 'font-lock-type-face)
               "]"
               "["
               (propertize (format-time-string "%Y-%m-%d") 'face 'font-lock-type-face)
               "]"

               ;;global-mode-string, org-timer-set-timer in org-mode need this
               (propertize "%M" 'face nil)
               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 3 (string-width mode-name)))))))

               ;; the current major mode
               (propertize " %m " 'face 'font-lock-string-face)))


(provide 'init-modeline)
