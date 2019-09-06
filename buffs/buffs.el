;; configuration for loading buffs

(require 'usability)
(require 'project-management)

;; org mode ... might as well be god-mode
(require 'org-buffs)

;; programming modes and customizations
(require 'scala-buffs)
(require 'common-lisp)
(require 'haskell)

(use-package rmsbolt)

(use-package yasnippet)

;; woot?
(use-package graphql-mode)

;; do some additional random configuration
(put 'dired-find-alternate-file 'disabled nil)

;; on to our hooks since all packages should be ready to be customized
(global-prettify-symbols-mode)

(provide 'buffs)
