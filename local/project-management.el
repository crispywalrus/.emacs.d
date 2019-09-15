;; project-management.el -*- lexical-binding: t -*-

(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  :bind-keymap (("S-p" . projectile-command-map)
                ("C-c p")))

(provide 'project-management)
