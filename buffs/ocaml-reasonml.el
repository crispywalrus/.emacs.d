;; ocaml-reasonml.el  -*- lexical-binding: t -*-

;; emacs configuration and editing mode for ocalm and reasonml programming. 

(require 'coding-support)
(require 'dune)
(require 'dune-flymake)

(use-package reason-mode)

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

;; it's annoying that it's not possible to preload any of the dune support and get auto mode selection for dune-project files.
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune-project\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'ocaml-reasonml)

