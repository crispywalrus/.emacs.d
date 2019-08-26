(use-package haskell-mode
  :ensure t
  :commands haskell-mode)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint)))))

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-configure 
  :init 
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))

(provide 'haskell-pack)
