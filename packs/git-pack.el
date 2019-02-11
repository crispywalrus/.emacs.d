(use-package with-editor
  :pin melpa-stable)

(use-package git-commit
  :pin melpa-stable)

(use-package magit
  :pin melpa-stable
  :commands magit-status magit-blame
  :init
  (setq magit-auto-revert-mode nil
        magit-last-seen-setup-instructions "1.4.0")
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-timemachine)

(provide 'git-pack)
