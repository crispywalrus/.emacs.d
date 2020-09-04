;; configure some stuff to make dealing with docker easier, if not
;; pleasant.

(use-package docker)

(use-package docker-api)

(use-package docker-cli)

(use-package docker-compose-mode
  :hook yaml-mode)

(provide 'containers)
