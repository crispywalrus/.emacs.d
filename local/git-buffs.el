;; super power buff for git interactions
;;
;; this uses magit for basic git status and commands, with added fun
;; from magithub for our github buffers, and git-timemachine to
;; retrieve, view, and even edit the past

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

(use-package git-timemachine
  :commands git-timemachine git-timemachine-toggle
  :bind (("s-t" . git-timemachine)
         ("s-T" . git-timemchine-toggle)))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(provide 'git-buffs)
