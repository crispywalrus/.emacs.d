;; configure for scala coding pleasure
;;
;; sadly ensime is falling back so here i am falling back to lsp.

(require 'coding-core)

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

(use-package ob-ammonite
  :config
  (setq ob-ammonite-prompt-string "@"))

(require 'git-buffs)
(provide 'scala-buffs)
