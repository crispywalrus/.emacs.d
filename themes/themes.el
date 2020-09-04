;; themes.el, load some themes to choose between -*- lexical-binding: t -*-

;; (use-package nyx-theme)
;; (use-package material-theme)
;; (use-package immaterial-theme)
;; (use-package cyberpunk-theme)
;; (use-package apropospriate-theme)
;; (use-package paganini-theme)

;; (use-package nyan-mode
;;   :init (nyan-mode 1))

(use-package all-the-icons)

(use-package spaceline)

;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :init (spaceline-all-the-icons-theme))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'themes)
