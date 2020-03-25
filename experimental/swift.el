;; swift.el -- configuration for swift development -*- lexical-binding: t -*-

(use-package swift)

(use-package flycheck-swift)

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "<path_to_sourcekit-lsp_executable>")))

(provide 'swift)
