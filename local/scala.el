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

(use-package lsp-mode
  :hook (scala-mode . lsp)
  :init (setq lsp-prefer-flymake nil
              lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp)

(provide 'scala)
