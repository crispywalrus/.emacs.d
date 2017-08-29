
;; ocalm et. al.
(use-package merlin)
(use-package tuareg
  :init (setq merlin-command 'opam)
  :config (add-hook 'tuarag-model-hook (lambda ()
                                         (merlin-mode t)
                                         (utop-minor-mode))))

(use-package utop)

(provide 'ocaml-pack)
