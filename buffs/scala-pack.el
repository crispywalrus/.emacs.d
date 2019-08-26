
(use-package sbt-mode
  :init (setq sbt:prefer-nested-projects t)
  :commands sbt-start sbt-command sbt-hydra)

(use-package scala-mode
  :after sbt-mode
  :interpreter ("scala" . scala-mode)
  :hook (lambda ()
          (setq prettify-symbols-alist scala-prettify-symbols-alist)
          (smartparens-mode t))
  :bind (:map scala-mode-map
              ("C-c C-b" . sbt-hydra)))

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-scala
  :after scala-mode
  :demand t
  :hook (scala-mode . lsp))

(use-package popup
  :pin melpa-stable)

(use-package company-lsp)

(use-package ob-ammonite
  :config
  (setq ob-ammonite-prompt-string "@"))

(require 'tree)
(require 'git-pack)
(provide 'scala-pack)
