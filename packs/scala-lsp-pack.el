
(use-package sbt-mode
  :pin melpa-stable
  :commands sbt-start sbt-command
  :init (setq sbt:prefer-nested-projects t))

(use-package scala-mode
  :pin melpa-stable
  :chords ((":." . ":.")
           ("=>" . "⇒")
           ("->" . "→")
           ("<-" . "←")
           ("<." . "≤")
           (">." . "≥")
           ("==" . "≡"))
  :interpreter ("scala" . scala-mode)
  :hook (lambda ()
          (setq prettify-symbols-alist scala-prettify-symbols-alist)
          (smartparens-mode t)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-scala
  :after scala-mode
  :demand t
  :hook (scala-mode . lsp))

(provide 'scala-lsp-pack)
