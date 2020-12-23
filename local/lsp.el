;; lsp.el -- enable lsp-mode and lsp-ui -*- lexical-binding: t -*-

(require 'coding)

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil
              lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package company-lsp
  :init (push 'company-lsp company-backends))


(provide 'lsp)
