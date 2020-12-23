;; completion.el -- enable completion in some buffers -*- lexical-binding: t -*-
;;
(require 'project-management)

(use-package company
  :ensure t
  :config
    (add-hook 'after-init-hook 'global-company-mode))

(provide 'completion)
