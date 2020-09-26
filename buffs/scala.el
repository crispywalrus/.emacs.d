;; scala.el -- configure for scala coding pleasure -*- lexical-binding: t -*-

(require 'coding-support)

(use-package lsp-metals)

(use-package sbt-mode
  :init (setq sbt:prefer-nested-projects t)
  :commands sbt-start sbt-command sbt-hydra
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package scala-mode
  :config
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  :init
  (add-hook 'scala-mode-hook 'company-mode)
  :bind
  ("C-c C-b" . sbt-hydra))

(use-package ob-ammonite
  :config
  (setq ob-ammonite-prompt-string "@"))

(provide 'scala)
