;; configuration for loading buffs

(require 'usability)
(require 'mac-keys)

(require 'project-management)
(require 'coding)

;; org mode ... might as well be god-mode
(require 'org-buffs)

;; programming modes and customizations
(require 'git-buffs)
(require 'scala)
(require 'common-lisp)
(require 'haskell)
(require 'markup)
(require 'ocaml-reasonml)
(require 'containers)
(require 'idl)

(use-package rmsbolt)

(require 'logic)

;; do some additional random configuration
(put 'dired-find-alternate-file 'disabled nil)

;; on to our hooks since all packages should be ready to be customized
(global-prettify-symbols-mode)

(provide 'buffs)
