;; coding-support.el -*- lexical-binding: t -*- this is all the things

;; needed to get the background things running and configured in order
;; to make modern programming environments work. mostly this means
;; getting lsp-mode all happy.

(use-package lsp-mode
  :config (setq lsp-enable-snippet nil)
  :hook (scala-mode . lsp)
        (lsp-mode . lsp-lens-mode))

(use-package lsp-ui)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package posframe)

(use-package company-posframe
  :config (company-posframe-mode 1))

;; (use-package hydra-posframe
;;   :hook (after-init . hydra-posframe-enable))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

(provide 'coding-support)
