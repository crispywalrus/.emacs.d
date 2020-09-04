;; idl.el support various idl languages -*- lexical-bindings: t -*-

(use-package thrift)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package protobuf-mode)

(use-package graphql-mode)


(provide 'idl)

