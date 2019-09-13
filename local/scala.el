;; configure for scala coding pleasure

(use-package sbt-mode
  :init (setq sbt:prefer-nested-projects t)
  :commands sbt-start sbt-command sbt-hydra
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package scala-mode
  :config (setq prettify-symbols-alist scala-prettify-symbols-alist)
  :hook (smartparens-mode))

(use-package lsp-scala
  :after scala-mode
  :demand t
  :hook (scala-mode . lsp))

(use-package ob-ammonite
  :config
  (setq ob-ammonite-prompt-string "@"))

(provide 'scala)
