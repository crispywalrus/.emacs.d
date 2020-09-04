;; scala.el -- configure for scala coding pleasure -*- lexical-binding: t -*-

(use-package sbt-mode
  :init (setq sbt:prefer-nested-projects t)
  :commands sbt-start sbt-command sbt-hydra
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package scala-mode
  :config (setq prettify-symbols-alist scala-prettify-symbols-alist)
  :bind ("C-c C-b" . sbt-hydra))

(use-package ob-ammonite
  :config
  (setq ob-ammonite-prompt-string "@"))

(use-package lsp-metals
  :hook (scala-mode . lsp))

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(provide 'scala)
