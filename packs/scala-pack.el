
(use-package yasnippet)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :init (setq sbt:prefer-nested-projects t))


(use-package scala-mode
  :chords (("=>" . "⇒")
           ("->" . "→")
           ("<-" . "←"))
  :interpreter ("scala" . scala-mode)
  :hook (lambda ()
          (setq prettify-symbols-alist scala-prettify-symbols-alist)
          (smartparens-mode t)))

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

(use-package ensime
  :pin melpa-stable
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-startup-snapshot-notification nil
   ensime-startup-notification nil)
  :config
  (require 'ensime-expand-region)
  :hook (ensime-mode . (lambda ()
          (let ((backends (company-backends-for-buffer)))
            (setq company-backends
                  (push '(ensime-company company-yasnippet) backends))))))


(provide 'scala-pack)
