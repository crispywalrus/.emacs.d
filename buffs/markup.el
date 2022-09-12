;; 
;; manipulating markdown (mostly) or any markup format that's not org
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
    
(use-package pandoc-mode
  :hook markdown-mode)

(use-package yaml)
(use-package yaml-pro
  :hook yaml-mode)

;; (use-package yaml-mode)

(provide 'markup)
