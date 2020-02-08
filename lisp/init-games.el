;;; init-games.el --- -*- lexical-binding: t -*-

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes tetris, speed-type, 2048
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code

;; TetrisConfig
(use-package tetris
  :ensure nil
  :commands (tetris)
  :bind
  (:map tetris-mode-map
   ("C-p" . tetris-rotate-prev)
   ("C-n" . tetris-rotate-down)
   ("C-b" . tetris-move-left)
   ("C-f" . tetris-move-right)
   ("C-SPC" . tetris-move-bottom))
  :config
  (defadvice tetris-end-game (around zap-scores activate)
    (save-window-excursion ad-do-it)))



(use-package speed-type
  :commands (speed-type-text))



(use-package 2048-game
  :commands (2048-game))


(provide 'init-games)

;;; init-games.el ends here.
