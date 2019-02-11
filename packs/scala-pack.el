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
  :interpreter ("scala" . scala-mode))

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
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0))))

(add-hook 'scala-mode-hook
          (lambda ()
            (setq prettify-symbols-alist scala-prettify-symbols-alist)
            (smartparens-mode t)))

(add-hook 'ensime-mode-hook
          (lambda ()
            (let ((backends (company-backends-for-buffer)))
              (setq company-backends
                    (push '(ensime-company company-yasnippet) backends)))))

(provide 'scala-pack)
