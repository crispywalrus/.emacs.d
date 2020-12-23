
;; programming modes and customizations
(require 'git-buffs)
(require 'scala)
(require 'markup)
(require 'ocaml-reasonml)
(require 'containers)
(require 'idl)
(require 'haskell)

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

;; prettify all our codes
(global-prettify-symbols-mode)


(provide 'coding)
