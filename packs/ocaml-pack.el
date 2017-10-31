;; ocalm et. al.
(use-package merlin)
(use-package utop)
(use-package tuareg
  :config
  (add-hook 'tuarag-model-hook (lambda ()
                                 (utop-minor-mode))))

;; (use-package utop)
(provide 'ocaml-pack)
