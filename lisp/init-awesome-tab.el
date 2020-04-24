;;; init-awesomeTab.el ---Better Tab-bar -*- lexical-binding: t; -*-

;; Copyright (C) 2020  T_Fighting

;; Author: T_Fighting <545298210@qq.com>
;;; Keywords: Tab-bar

(use-package awesome-tab
	:ensure nil
	:hook
	(after-init . awesome-tab-mode)
	:config
	(setq awesome-tab-show-tab-index t
				awesome-tab-height 125)

	(global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
	(global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)

	)



(provide 'init-awesome-tab)

;;; init-awesome-tab.el ends here.
