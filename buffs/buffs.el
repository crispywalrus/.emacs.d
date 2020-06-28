;; configuration for loading buffs
(require 'code)
(require 'usability)
(require 'mac-keys)
;; org mode ... might as well be god-mode
(require 'org-buffs)

;; programming modes and customizations
(require 'git-buffs)
(require 'scala)
(require 'common-lisp)
(require 'haskell)
(require 'markup)
(use-package graphql-mode)
(use-package rmsbolt)

;; do some additional random configuration
(put 'dired-find-alternate-file 'disabled nil)
(global-prettify-symbols-mode)

(provide 'buffs)
