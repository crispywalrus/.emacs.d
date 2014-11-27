;; crispy's package manager. tracks and installs the crispy selected
;; suite of elisp packages. This is configuration as code that gets
;; pushed to a (public) github repo.

(require 'package)
(package-initialize)

(defun make-packages-installed ( packages )
  (progn
    (package-refresh-contents)
    (mapc 'unless-package-installed packages )))

(defun unless-package-installed (package)
  (unless (package-installed-p package)
     (package-install package)))
