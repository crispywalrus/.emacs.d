
(use-package thrift)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package protobuf-mode)

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :bind-keymap (("\C-m" . newline-and-indent)))

(provide 'idl)

