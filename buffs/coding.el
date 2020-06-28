
(use-package popup)

(use-package lsp-mode
  :hook (scala-mode . lsp))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp)

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package yasnippet)
(use-package yasnippet-snippets)

(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

(provide 'coding)
