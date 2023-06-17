;;; init.el --- emacs configuration -*- lexical-binding: t -*-

;; Copyright © 2011 - 2021 Chris Vale
;; Author: Chris Vale <crispywalrus@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; preable, use the built in package manager to bootstrap our package
;; management which is based on use-package
(require 'package)

;; add melps and org to the package archives we use
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

;; and finaglly initialize package manager
(package-initialize)


;; the rest of this uses use-package to manage loading and configuring
;; packagess. if use-package isn't installed go fetch and install
;; it. this is super easy because our just configured package manager
;; can fetch use package for us.
(when
    (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package download all referenced but uninstalled
;; packages without human intervention.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; for some reason the mac version of emacs has decided to use / as
;; the default directory. That's not great for usability.
(setq default-directory "~/")

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el" t)
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(load (expand-file-name "~/.emacs.d/configuration.el"))
;;; init.el ends here
