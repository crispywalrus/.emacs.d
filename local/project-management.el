;; project-management.el -*- lexical-binding: t -*-

(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

(provide 'project-management)
