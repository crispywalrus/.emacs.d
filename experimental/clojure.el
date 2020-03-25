;; clojure.el -- configure for pleasant clojure coding -*- lexical-binding: t -*-

(use-package clojure-mode)

(use-package ob-clojurescript)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :init
  (setq lsp-enable-indentation nil)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp))

(use-package lsp-mode
  :hook (scala-mode . lsp)
  :init (setq lsp-prefer-flymake nil
              lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :commands company-lsp)

(provide 'scala)
