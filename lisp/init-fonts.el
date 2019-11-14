;; init-font.el --- Setup fonts.  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (catch 'loop
    (dolist (font '("Fira Code" "SF Mono" "Hack" "Source Code Pro"
                    "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"))
      (when (member font (font-family-list))
        (set-face-attribute 'default nil :font font :height (cond
                                                             (sys/mac-x-p 130)
                                                             (t 125)))
        (throw 'loop t))))

  ;; Specify font for all unicode characters
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (throw 'loop t))))

  ;; Specify font for Chinese characters
  (catch 'loop
    (dolist (font '("WenQuanYi Micro Hei" "Microsoft Yahei"))
      (when (member font (font-family-list))
        (set-fontset-font t '(#x4e00 . #x9fff) font)
        (throw 'loop t)))))


(provide 'init-fonts)



;;; init-fonts ends here.
;;
