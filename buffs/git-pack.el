(use-package gh)

(use-package with-editor)

(use-package git-commit)

(use-package magit
  :commands magit-status magit-blame
  :init
  (setq magit-auto-revert-mode nil
        magit-last-seen-setup-instructions "1.4.0")
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-timemachine)

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(provide 'git-pack)
