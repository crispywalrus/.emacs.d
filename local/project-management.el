;;

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(provide 'project-management)
