;; ocalm et. al.
(require 'dune)
(use-package reason-mode)

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(provide 'ocaml-pack)

