
(use-package popup)

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package yasnippet)
(use-package yasnippet-snippets)

(provide 'coding)
