;; enhanced undo ... perhaps with peristence -*- lexical-binding: t -*-

(use-package undo-tree
  :ensure t
  :init
  (setq undo-limit 78643200)
  (setq undo-outer-limit 104857600)
  (setq undo-strong-limit 157286400)
  (setq undo-tree-mode-lighter " UN")
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-history-directory-alist '(("." . "~/emacs.d/undo")))
 (add-hook 'undo-tree-visualizer-mode-hook (lambda ()
                                              (undo-tree-visualizer-selection-mode)
                                              (setq display-line-numbers nil)))
  :config
  (global-undo-tree-mode 1))

(provide 'undo)
