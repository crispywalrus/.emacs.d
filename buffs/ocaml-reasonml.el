;; ocalm and reasonml
(require 'coding)
(require 'dune)
(require 'dune-flymake)

(use-package reason-mode)

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune-project\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'ocaml-reasonml)

